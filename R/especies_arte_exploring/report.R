library("R6")
library("assertr")
library("dplyr")
library("data.table")

EspeciesArteReport <- R6Class("EspeciesArteReport", public = list(
  db_data = NULL,
  summary = NULL,
  num_decimals = NULL,
  initialize = function(db_data, num_decimals) {
    self$db_data <- copy(db_data)
    self$num_decimals <- 5
  },
  generate_summary = function() {

    # (1) Get total number of rows per ESPECIE and ARTE
    esp_arte_rows <- db_data_tallas %>%
      group_by(ESPECIE, ARTE) %>%
      summarise(num_rows_arte_especies = n())


    # (2) Get total numbers by ESPECIE, by ESPECIE and ARTE and percentages by ESPECIE and ESPECIE and ARTE. Then add
    # the cumulative sum by ESPECIE and ARTE in descending order
    esp_arte_individuos <- db_data_tallas %>%
      filter(not_na(NUMINDIVS)) %>%
      group_by(ESPECIE, ARTE) %>%
      summarise(num_ind_arte_especie = sum(NUMINDIVS)) %>%
      mutate(num_ind_especie = sum(num_ind_arte_especie),
             arte_especie_fraction = round(num_ind_arte_especie / sum(num_ind_arte_especie), self$num_decimals) * 100) %>%
      arrange(ESPECIE, desc(arte_especie_fraction)) %>%
      mutate(arte_especie_cum = cumsum(arte_especie_fraction)) %>%
      ungroup() %>%
      mutate(especie_fraction = round(num_ind_especie / sum(num_ind_arte_especie), self$num_decimals) * 100)

    #  (3) Get cumulative by especies in descending order
    especies_cumsum <- esp_arte_individuos %>%
      distinct(ESPECIE, especie_fraction, .keep_all = TRUE) %>%
      arrange(desc(especie_fraction)) %>%
      mutate(especie_cum = round(cumsum(especie_fraction)), self$num_decimals) %>%
      select(ESPECIE, especie_cum)

    # (self$num_decimals) Ensure there are no NAs in the rows
    mute <- esp_arte_rows %>%
      assert(not_na, colnames(.))
    mute <- esp_arte_individuos %>%
      assert(not_na, colnames(.))
    mute <- especies_cumsum %>%
      assert(not_na, colnames(.))

    # (5) join all matrices together into one
    self$summary <- merge(esp_arte_rows,
                          esp_arte_individuos,
                          by = c("ESPECIE", "ARTE"),
                          all = TRUE) %>%
      select(ESPECIE, ARTE, num_ind_especie, num_rows_arte_especies, num_ind_arte_especie, arte_especie_fraction,
             arte_especie_cum, especie_fraction)

    self$summary <- merge(self$summary, especies_cumsum, by = "ESPECIE", all = TRUE) %>%
      arrange(desc(especie_fraction), ESPECIE, desc(arte_especie_fraction))

  },

  add_arte_nicknames = function() {
    arte_nick_name_df <- self$summary %>%
      select(ARTE) %>%
      distinct() %>%
      rowwise() %>%
      mutate(arte_nickname = case_when(
        # Only one word larger than 6 ~ get three characters
        length(unlist(str_split(ARTE, " "))) == 1 & nchar(ARTE) > 6 ~
          paste(substring(unlist(str_split(ARTE, " ")), 1, 3), collapse = "_"),
        nchar(ARTE) <= 6 ~ ARTE,
        .default = paste(substring(unlist(str_split(ARTE, " ")), 1, 2), collapse = "_")
      )) %>%
      mutate(
        arte_nickname = case_when(
          arte_nickname == 'RA_VI_VO_ZA_OS' ~ 'RA_MULTI',
          .default = arte_nickname
        )
      )
    mute <- arte_nick_name_df %>%
      verify(length(unique(ARTE)) == length(unique(arte_nickname)))

    self$summary <- merge(self$summary, arte_nick_name_df, by = "ARTE", all = TRUE) %>%
      relocate(arte_nickname, .after = ARTE) %>%
      arrange(desc(especie_fraction), ESPECIE, desc(arte_especie_fraction))
  },
  get_most_representative_arte = function(data, threshold, other_keyword) {

    # Get all ARTEs per ESPECIE which total cumulative sum(%) is above a given threshold
    closest_to_threshold <- data %>%
      select(ESPECIE, ARTE, arte_especie_fraction, arte_especie_cum) %>%
      group_by(ESPECIE) %>%
      filter(arte_especie_cum > threshold) %>%
      filter(arte_especie_cum - threshold == min(arte_especie_cum - threshold)) %>%
      rename(min_cum_threshold_arte_especie = arte_especie_cum) %>%
      select(-c(arte_especie_fraction, ARTE))

    # (2) Rename those ARTEs that lay within (100 - threshold)% as a given keyword. They aren't important!
    data <- merge(data, closest_to_threshold, by = "ESPECIE", all = TRUE) %>%
      mutate(ARTE = case_when(
        arte_especie_cum > min_cum_threshold_arte_especie ~ other_keyword,
        .default = ARTE
      ), arte_nickname = case_when(
        ARTE == other_keyword ~ other_keyword,
        .default = arte_nickname
      )) %>%
      arrange(desc(especie_fraction), ESPECIE, desc(arte_especie_fraction))


    # (3) Split main ARTEs from the non-relevant ones and calculate the contribution of the latter to the total sum (%)
    common_columns <- c('ESPECIE', 'ARTE', 'arte_nickname', 'arte_especie_fraction', 'especie_fraction')
    main_ARTE <- data %>%
      filter(ARTE != other_keyword) %>%
      select_at(.vars = common_columns)

    remaining_arte_summed_up <- data %>%
      filter(ARTE == other_keyword) %>%
      select_at(.vars = common_columns) %>%
      group_by(ESPECIE) %>%
      mutate(arte_especie_fraction = sum(arte_especie_fraction)) %>%
      distinct()

    # Last
    summary_especie_arte <- rbind(main_ARTE, remaining_arte_summed_up) %>%
      arrange(ESPECIE, -arte_especie_fraction)

    return(summary_especie_arte)
  }

))
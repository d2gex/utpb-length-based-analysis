library("R6")
library("assertr")
library("dplyr")
library("data.table")
source("especies_arte_exploring/base_report.R")

EspeciesArteReport <- R6Class("EspeciesArteReport", inherit = Report, public = list(
  summary_up_to_threshold = NULL,
  summary_from_threshold = NULL,
  num_decimals = NULL,
  initialize = function(db_data, num_decimals) {
    super$initialize(db_data, num_decimals)
  },
  generate_summary = function() {
    # // @formatter:off
    #' Get a long dataframe with osummarised details from the tandem ESPECIE-ARTE perspective for the whole
    #' timeseries
    #' @return:
    #'  - ESPECIE
    #'  - ARTE
    #'  - num_ind_especie: number of individuals by species only
    #'  - especie_fraction: % by species only
    #'  - num_rows_arte_especies: number of rows in the matrix by species-gear
    #'  - num_ind_arte_especie: number of individuals by species-arte
    #'  - arte_especie_fraction: % by species-arte
    #'  - arte_especie_cum: cumulative sum by arte_especie_fraction
    # // @formatter:on

    # (1) Get total number of rows per ESPECIE and ARTE
    esp_arte_rows <- self$db_data %>%
      group_by(ESPECIE, ARTE) %>%
      summarise(num_rows_arte_especies = n())
    mute <- esp_arte_rows %>%
      assert(not_na, colnames(.))


    # (2) Get total numbers by ESPECIE, by ESPECIE and ARTE and percentages by ESPECIE and ESPECIE and ARTE. Then add
    # the cumulative sum by ESPECIE and ARTE in descending order
    esp_arte_individuos <- self$db_data %>%
      filter(not_na(NUMINDIVS)) %>%
      group_by(ESPECIE, ARTE) %>%
      summarise(num_ind_arte_especie = sum(NUMINDIVS)) %>%
      mutate(num_ind_especie = sum(num_ind_arte_especie),
             arte_especie_fraction = round(num_ind_arte_especie / sum(num_ind_arte_especie), self$num_decimals) * 100) %>%
      arrange(ESPECIE, desc(arte_especie_fraction)) %>%
      mutate(arte_especie_cum = cumsum(arte_especie_fraction)) %>%
      ungroup() %>%
      mutate(especie_fraction = round(num_ind_especie / sum(num_ind_arte_especie), self$num_decimals) * 100)

    mute <- esp_arte_individuos %>%
      assert(not_na, colnames(.))
    #  (3) Get cumulative by especies in descending order
    especies_cumsum <- esp_arte_individuos %>%
      distinct(ESPECIE, especie_fraction, .keep_all = TRUE) %>%
      arrange(desc(especie_fraction)) %>%
      mutate(especie_cum = round(cumsum(especie_fraction)), self$num_decimals) %>%
      select(ESPECIE, especie_cum)

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

  split_summary_by_threshold = function(species_threshold, gears_threshold, gears_other_keyword) {
    # @formatter:off
    #' Given an overall summary dataframe,  it splits the dataset into two subset: the first one will contain
    #' up to 'species_threshold' of all species that contribute to the sampling and the second one the remaining. Both
    #' subsets are reworked so that all gears that contribute the least are grouped and rename as 'gears_other_keyword'
    #' and those up to 'gears_threshold' are left as such.
    # @formatter:on

    up_to_threshold <- self$summary %>% filter(especie_cum <= species_threshold)
    from_threshold <- self$summary %>% filter(especie_cum > species_threshold)
    self$summary_up_to_threshold <- private$get_arte_up_to_threshold(up_to_threshold, gears_threshold, gears_other_keyword)
    self$summary_from_threshold <- private$get_arte_up_to_threshold(from_threshold, gears_threshold, gears_other_keyword)
  }

), private = list(
  get_arte_up_to_threshold = function(data, threshold, other_keyword) {
    # @formatter:off
    #' Given an overall summary dataframe, calculate the gears that count for a given % threshold of the
    #' total sampling. The remaining from the threshold up to 100% are labelled with a keyword (normallly 'Rest',
    #' 'Other' etc..
    # @formatter:on

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
      select(ESPECIE, ARTE, arte_nickname, arte_especie_fraction, especie_fraction, num_ind_especie)

    remaining_arte_summed_up <- data %>%
      filter(ARTE == other_keyword) %>%
      select(ESPECIE, ARTE, arte_nickname, arte_especie_fraction, especie_fraction, num_ind_especie) %>%
      group_by(ESPECIE) %>%
      mutate(arte_especie_fraction = sum(arte_especie_fraction)) %>%
      distinct()

    # Last
    summary_especie_arte <- rbind(main_ARTE, remaining_arte_summed_up) %>%
      mutate(arte_especie_absolute_fraction = (arte_especie_fraction / 100) * especie_fraction) %>%
      arrange(-arte_especie_absolute_fraction, ESPECIE)

    return(summary_especie_arte)
  }
))

library("R6")
library("assertr")
library("dplyr")

EspeciesArteReport <- R6Class("EspeciesArteReport", public = list(
  db_data = NULL,
  summary = NULL,
  initialize = function(db_data) {
    self$db_data <- db_data
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
             arte_especie_fraction = round(num_ind_arte_especie / sum(num_ind_arte_especie), 2) * 100) %>%
      arrange(ESPECIE, desc(arte_especie_fraction)) %>%
      mutate(arte_especie_cum = cumsum(arte_especie_fraction)) %>%
      ungroup() %>%
      mutate(especie_fraction = round(num_ind_especie / sum(num_ind_arte_especie), 2) * 100)

    #  (3) Get cumulative by especies in descending order
    especies_cumsum <- esp_arte_individuos %>%
      distinct(ESPECIE, especie_fraction, .keep_all = TRUE) %>%
      arrange(desc(especie_fraction)) %>%
      mutate(especie_cum = round(cumsum(especie_fraction)), 2) %>%
      select(ESPECIE, especie_cum)

    # (4) Ensure there are no NAs in the rows
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

  }
))
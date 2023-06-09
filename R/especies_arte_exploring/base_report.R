library("R6")

Report <- R6Class("Report", public = list(
  db_data = NULL,
  summary = NULL,
  initialize = function(db_data, num_decimals) {
    self$db_data <- copy(db_data)
    self$num_decimals <- num_decimals
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
  generate_summary = function() { }
))
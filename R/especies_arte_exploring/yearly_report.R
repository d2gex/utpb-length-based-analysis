library("R6")
source("especies_arte_exploring/base_report.R")

EspeciesArteYearReport <- R6Class("EspeciesArteYearReport", inherit = Report, public = list(
  initialize = function(db_data, num_decimals, overall_summary) {
    super$initialize(db_data, num_decimals)
    self$overall_summary <- overall_summary
  },
  generate_summary = function() {

  }
), private = list(
  generate_summary_single_specie = function(specie) {
    specie_df <- self$db_data %>% filter(ESPECIE == specie)


  }
))
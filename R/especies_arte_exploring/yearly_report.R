library("R6")
source("especies_arte_exploring/base_report.R")

EspeciesArteYearReport <- R6Class("EspeciesArteYearReport", inherit = Report, public = list(
  initialize = function(db_data, num_decimals, overall_summary) {
    super$initialize(db_data, num_decimals)
  },
  generate_summary = function() {

  }
), private = list(
  get_summary_by_specie = function(specie) {

  }
))
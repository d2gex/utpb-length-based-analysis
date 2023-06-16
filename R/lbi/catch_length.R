library("R6")
library("dplyr")

CatchAtLength <- R6Class("CatchAtLength", public = list(
  species_data = NULL,
  initialize = function(db_data, specie) {
    self$species_data <- db_data %>% filter(ESPECIE = specie)
  },

  build_length_composition_matrix = function(bind_width) {

  }
))



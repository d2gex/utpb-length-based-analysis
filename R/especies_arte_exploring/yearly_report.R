library("R6")
source("especies_arte_exploring/base_report.R")

EspeciesArteYearReport <- R6Class("EspeciesArteYearReport", inherit = Report, public = list(
  overall_summary = NULL,
  other_keyword = NULL,
  initialize = function(db_data, num_decimals, overall_summary, other_keyword) {
    super$initialize(db_data, num_decimals)
    self$overall_summary <- overall_summary
    self$other_keyword <- other_keyword
  },
  generate_summary = function() {
    species <- unique(self$overall_summary$ESPECIE)
    species_data <- lapply(species,
                           function(specie) private$generate_summary_single_specie(specie))
    summary <- list()
    for (x in 1:length(species)) {
      summary[species[x]] <- species_data[x]
    }
    self$summary <- summary
  }
), private = list(
  generate_summary_single_specie = function(specie_name) {
    print(paste("-------------->", specie_name))
    # // @formatter:off
    #' Generate the yearly summary for a single species and main gears represented in the sampling

    # (1) Make sure the dataframe as is as long as the total sume of NUMINDIVS so that one sinle individual
    # takes up one single row
    specie_df <- self$db_data %>%
      filter(ESPECIE == specie_name) %>%
      select(ESPECIE, TALLA, year, NUMINDIVS, ARTE) %>%
      uncount(NUMINDIVS)

    specie_individuals <- sum((self$db_data %>% filter(ESPECIE == specie_name))$NUMINDIVS)
    mute <- specie_df %>%
      verify(nrow(.) == specie_individuals)

    # (2) Get a single species' mains ARTE and num_individuals by ARTE
    specie_summary_df <- self$overall_summary %>%
      filter(ESPECIE == specie_name)
    specie_artes <- setdiff(unique(specie_summary_df$ARTE), self$other_keyword)
    specie_relevant_arte_df <- specie_df %>%
      filter(ARTE %in% specie_artes)

    # (3) Calculate yearly abundance per ARTE
    species_relevant_arte_numinds_df <- specie_relevant_arte_df %>%
      filter(ARTE %in% specie_artes) %>%
      group_by(year, ARTE) %>%
      summarise(year_arte_abundance = n())

    # (4) Ensure that abundances have been properly calculated
    mute <- species_relevant_arte_numinds_df %>%
      summarise(year_abundance = sum(year_arte_abundance)) %>%
      summarise(total_year_abundance = sum(year_abundance)) %>%
      verify(total_year_abundance == nrow(specie_relevant_arte_df))

    # (5) Calculate yearly TALLA per ARTE
    species_relevant_arte_meantalla_df <- specie_relevant_arte_df %>%
      group_by(year, ARTE) %>%
      summarise(mean_year_arte_talla = round(mean(TALLA), 2)) %>%
      arrange(year, ARTE)

    # (6) Merge yearly average TALLA and number of individuals
    specie_data <- merge(species_relevant_arte_numinds_df,
                         species_relevant_arte_meantalla_df,
                         by = c("year", "ARTE"),
                         all = TRUE)
    # (7) Add missing ARTE for all years
    specie_data <- as.data.frame(specie_data %>%
      complete(year, ARTE, fill = list(year_arte_abundance = 0,  mean_year_arte_talla = 0), explicit = FALSE))
    print(paste("-------------->", "EMD"))
    return(specie_data)

  }
))
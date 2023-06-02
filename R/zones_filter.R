source("config.R")
library("R6")

ZoneFilter <- R6Class("ZoneFilter",
                      inherit = BaseDataFilter,
                      public = list(
                        admin_zones = NULL,
                        initialize = function(clean_data, dirty_data) {
                          super$initialize(clean_data, dirty_data)
                          self$admin_zones <- NewAdministrativeZones$new()
                        },
                        define_admin_zones = function() {
                          private$define_new_admin_zones()
                          private$transform_named_zones_to_admin_zones()
                          private$define_oceanographic_zones()
                          private$define_ices_zone()
                          invisible(self)
                        }
                      ),
                      private = list(
                        transform_named_zones_to_admin_zones = function() {
                          self$clean_df <- self$clean_df %>%
                            mutate(
                              admin_zone = case_when(
                                is.na(admin_zone) & ZONA == "Zona I - Vigo" ~ 1,
                                is.na(admin_zone) & ZONA == "Zona II - Pontevedra" ~ 2,
                                is.na(admin_zone) & ZONA == "Zona III - Arousa" ~ 3,
                                is.na(admin_zone) & ZONA == "Zona IV - Muros" ~ 4,
                                is.na(admin_zone) & ZONA == "Zona V - Fisterra" ~ 5,
                                is.na(admin_zone) & ZONA == "Zona VI - Costa da Morte" ~ 6,
                                is.na(admin_zone) & ZONA == "Zona VII - Coruna-Ferrol" ~ 7,
                                is.na(admin_zone) & ZONA == "Zona VIII - Cedeira" ~ 8,
                                is.na(admin_zone) & ZONA == "Zona IX - Mariña lucense" ~ 9,
                                .default = admin_zone
                              )
                            )
                        },
                        define_new_admin_zones = function() {
                          # Zona 9 (A Mariña)
                          self$clean_df <- self$clean_df %>%
                            mutate(
                              admin_zone = case_when(
                                # Zona 9 (A Mariña)
                                lon >= self$admin_zones$BaresLon ~ 9,
                                # Zona 8 (Cedeira)
                                lon < self$admin_zones$BaresLon & lat >= 43.51 ~ 8,
                                # Zona 7 (Coruña-Ferrol)
                                lon >= self$admin_zones$LangosteiraLon & lat <= 43.51 ~ 7,
                                # Zona 6 (Costa da Morte)
                                lon < self$admin_zones$LangosteiraLon & lat >= self$admin_zones$TourinhanLat ~ 6,
                                # Zona 5 (Fisterra)
                                lat < self$admin_zones$TourinhanLat &
                                  lat >= self$admin_zones$InsuaLat &
                                  lon <= -9.08 ~ 5,
                                # Zona (4)
                                # --->(Muros)
                                lat < self$admin_zones$InsuaLat & lat >= self$admin_zones$SieiraLat ~ 4,
                                # --> (Interior Ria Muros)
                                lon > -9.08 &
                                  lon <= -8.8 &
                                  lat > self$admin_zones$InsuaLat &
                                  lat < 42.85 ~ 4,
                                # Zona 3
                                # --->(Interior Ria Muros)
                                lat < self$admin_zones$SieiraLat & lat >= self$admin_zones$FaxildaLat ~ 3,
                                # ---> (Interior Ria Arousa)
                                lon > -8.9 &
                                  lon <= -8.7 &
                                  lat > self$admin_zones$SieiraLat &
                                  lat < 42.7 ~ 3,
                                # Zona 2
                                # ---> (Pontevedra)
                                lat < self$admin_zones$FaxildaLat & lat >= self$admin_zones$SoavelaLat ~ 2,
                                # ---> (Interior Ria Pontevedra 1)
                                lon > -8.75 &
                                  lon <= -8.65 &
                                  lat > self$admin_zones$FaxildaLat &
                                  lat < 42.45 ~ 2,
                                # ---> (Interior Ria Pontevedra 2)
                                lon > -8.8229 &
                                  lon <= -8.822 &
                                  lat > 42.277 &
                                  lat < 42.278 ~ 2,
                                # Zona 1
                                # ---> (Vigo)
                                lat < self$admin_zones$SoavelaLat ~ 1,
                                # ---> (Interior Ria Vigo 1)
                                lon > -8.68 &
                                  lon <= -8.5 &
                                  lat > self$admin_zones$SoavelaLat &
                                  lat < 42.36 ~ 1,
                                # ---> (Interior Ria Vigo 2)
                                lon > -8.74 &
                                  lon <= -8.67 &
                                  lat > self$admin_zones$SoavelaLat &
                                  lat < 42.3 ~ 1,
                                 .default = NA
                              )
                            )
                        },
                        define_oceanographic_zones = function() {
                          self$clean_df <- self$clean_df %>% mutate(
                            oceano_zone = case_when(
                              admin_zone >= 8 ~ 3,
                              admin_zone == 7 | admin_zone == 6 ~ 2,
                              .default = 1
                            )
                          )
                        },
                        define_ices_zone = function() {
                          self$clean_df <- self$clean_df %>% mutate(
                            ices_zone = case_when(
                              oceano_zone == 1 ~ "9.a",
                              .default = "8.c"
                            )
                          )
                        }

                      )
)

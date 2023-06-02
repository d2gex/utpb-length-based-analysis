source("config.R")
library("R6")

ZoneFilter <- R6Class("ZoneFilter",
                      inherit = BaseDataFilter,
                      public = list(
                        initialize = function(clean_data, dirty_data) {
                          super$initialize(clean_data, dirty_data)
                          self$admin_zones <- NewAdministrativeZones$new()
                        },
                        define_admin_zones = function() {
                          private$transform_named_zones_to_admin_zones()
                          private$define_new_admin_zones()
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
                                ZONA == "Zona I - Vigo" ~ 1,
                                ZONA == "Zona II - Pontevedra" ~ 2,
                                ZONA == "Zona III - Arousa" ~ 3,
                                ZONA == "Zona IV - Muros" ~ 4,
                                ZONA == "Zona V - Fisterra" ~ 5,
                                ZONA == "Zona VI - Costa da Morte" ~ 6,
                                ZONA == "Zona VII - Corunha-Ferrol" ~ 7,
                                ZONA == "Zona VIII - Cedeira" ~ 8,
                                .default = 9
                              )
                            )
                        },
                        define_new_admin_zones = function() {
                          admin_zones <- NewAdministrativeZones$new()
                          # Zona 9 (A Mariña)
                          self$clean_df <- self$clean_df %>%
                            mutate(
                              admin_zone = case_when(
                                lon >= admin_zones$BaresLon ~ 9,
                                .default = 8
                              )
                            ) %>%
                            mutate(
                              admin_zone = case_when(
                                # Zona 8 (Cedeira)
                                lat < admin_zones.BaresLon & lat >= 43.51 ~ 8,
                                # Zona 7 (Coruña-Ferrol)
                                lon >= admin_zones.LangosteiraLon & lat <= 43.51 ~ 7,
                                # Zona 6 (Costa da Morte)
                                lon < admin_zones.LangosteiraLon & lat >= admin_zones.TourinhanLat ~ 6,
                                # Zona 5 (Fisterra)
                                lat < admin_zones.TourinhanLat &
                                  lat >= admin_zones.InsuaLat &
                                  lon <= -9.08 ~ 5,
                                # Zona (4)
                                # --->(Muros)
                                lat < admin_zones.Insualat & lat >= admin_zones.Sieiralat ~ 4,
                                # --> (Interior Ria Muros)
                                lon > -9.08 &
                                  lon <= -8.8 &
                                  lat > admin_zones.Insualat &
                                  lat < 42.85 ~ 4,
                                # Zona 3
                                # --->(Interior Ria Muros)
                                lat < admin_zones.Sieiralat & lat >= admin_zones.Faxildalat ~ 3,
                                # ---> (Interior Ria Arousa)
                                lon > -8.9 &
                                  lon <= -8.7 &
                                  lat > admin_zones.Sieiralat &
                                  lat < 42.7 ~ 3,
                                # Zona 2
                                # ---> (Pontevedra)
                                lat < admin_zones.Faxildalat & lat >= admin_zones.Soavelalat ~ 2,
                                # ---> (Interior Ria Pontevedra 1)
                                lon > -8.75 &
                                  lon <= -8.65 &
                                  lat > admin_zones.Faxildalat &
                                  lat < 42.45 ~ 2,
                                # ---> (Interior Ria Pontevedra 2)
                                lon > -8.8229 &
                                  lon <= -8.822 &
                                  lat > 42.277 &
                                  lat < 42.278 ~ 2,
                                # Zona 1
                                # ---> (Vigo)
                                lat < admin_zones.Soavelalat ~ 1,
                                # ---> (Interior Ria Vigo 1)
                                lon > -8.68 &
                                  lon <= -8.5 &
                                  lat > admin_zones.Soavelalat &
                                  lat < 42.36 ~ 1,
                                # ---> (Interior Ria Vigo 2)
                                lon > -8.74 &
                                  lon <= -8.67 &
                                  lat > admin_zones.Soavelalat &
                                  lat < 42.3 ~ 1,
                                .default = .
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




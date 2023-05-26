library("R6")
DATA_PATH <- "../data"
DATA_SENSITIVE_PATH <- file.path(DATA_PATH, 'sensitive')
DB_TALLAS_PATH <- file.path(DATA_SENSITIVE_PATH, 'CONSULTA BDP_UTPB_TALLAS_17-04-2023_.csv')
DB_READ <- FALSE


NewAdministrativeZones <- R6Class("NewAdministrativeZones",
                                  public = list(
                                    BaresLat = 43.791336,
                                    BaresLon = -7.688769,
                                    PriorinhoLat = 43.463275,
                                    PriorinhoLon = -8.348032,
                                    LangosteiraLat = 43.361084,
                                    LangosteiraLon = -8.486756,
                                    TourinhanLat = 43.059324,
                                    TourinhanLon = -9.293350,
                                    InsuaLat = 42.770942,
                                    InsuaLon = -9.127085,
                                    SieiraLat = 42.653324,
                                    SieiraLon = -9.042177,
                                    FaxildaLat = 42.414997,
                                    FaxildaLon = -8.881116,
                                    SoavelaLat = 42.277784,
                                    SoavelaLon = -8.864851
                                  ))
library("R6")
DATA_PATH <- "../../../repos_data/utpblbm/data"
QGIS_PATH <- "../../../repos_data/utpblbm/qgis"
EXTRA_DATA_PATH <- "../data"
DATA_SENSITIVE_PATH <- file.path(DATA_PATH, 'sensitive')
OLD_DBS_PATH <- file.path(DATA_SENSITIVE_PATH, 'consulta_utpb_2018')
DB_TALLAS_PATH <- file.path(DATA_SENSITIVE_PATH, 'CONSULTA BDP_UTPB_TALLAS_17-04-2023_.csv')
DB_CAPTURAS_PATH <- file.path(DATA_SENSITIVE_PATH, 'CONSULTA BDP_UTPB_CAPTURAS_12-04-2023.CSV')
DB_TALLAS_OLD_PATH <- file.path(OLD_DBS_PATH, 'CONSULTA BDP_UTPB_TALLAS_16-05-2018.csv')
DB_CAPUTRAS_OLD_PATH <- file.path(OLD_DBS_PATH, 'CONSULTA BDP_UTPB_CAPTURAS_16-05-2018.csv')
DATA_OUTPUT <- file.path(DATA_SENSITIVE_PATH, 'output')
QGIS_OUTPUT <- file.path(QGIS_PATH, 'output')
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

PlotContext <- R6Class("PlotContext", public = list(
  title = NULL,
  x_lab = NULL,
  y_lab = NULL,
  second_y_lab = NULL,
  legend_title = NULL,
  x_angle = NULL,
  title_size = NULL,
  face_text_size = NULL,
  x_text_size = NULL,
  y_text_size = NULL,
  legend_position = NULL
))

paper_type <- "a4r"
paper_height <- 8.268
paper_width <- 11.693
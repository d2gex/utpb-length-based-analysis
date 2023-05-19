DATA_PATH <- "../data"
DATA_SENSITIVE_PATH <- file.path(DATA_PATH, 'sensitive')
DB_TALLAS_PATH <- file.path(DATA_SENSITIVE_PATH, 'CONSULTA BDP_UTPB_TALLAS_17-04-2023_utf8.csv')
DB_READ <- FALSE
COL_NAME_MAPPING <- list(
  'LON.inicio' = 'start_long',
  'LAT.inicio' = 'start_lat',
  'LON.final' = 'end_long',
  'LAT.final' = 'end_lat'
)
library("readxl")
library("data.table")
library("openxlsx")
source("config.R")
source("old_vs_new_db_integrity/utils.R")

# (0) Read data
jun_2023_tallas_db <- read_excel(file.path(DATA_PATH, 'db_corrections', 'Consulta csic tallas 08-06-2023.xlsx'))
jun_2023_capturas_db <- read_excel(file.path(DATA_PATH, 'db_corrections', 'Consulta csic capturas 08-06-2023.xlsx'))
april_2018_tallas_db <- read_csv2(file.path(DATA_SENSITIVE_PATH,
                                            'consulta_utpb_2018',
                                            'CONSULTA BDP_UTPB_TALLAS_16-05-2018.csv'),
                                  locale = locale(encoding = 'latin1'))
april_2018_caputras_db <- read_csv2(file.path(DATA_SENSITIVE_PATH,
                                              'consulta_utpb_2018',
                                              'CONSULTA BDP_UTPB_CAPTURAS_16-05-2018.csv'),
                                    locale = locale(encoding = 'latin1'))



# (1) Generate spreadsheet with different in column names
talla_names_2023 <- sort(names(jun_2023_tallas_db))
talla_names_2018 <- sort(names(april_2018_tallas_db))
capturas_names_2023 <- sort(names(jun_2023_capturas_db))
capturas_names_2018 <- sort(names(april_2018_caputras_db))

# ---> Create dataframe with differences between tallas column names
cols <- make_cols_same_length(col_1 = talla_names_2023,
                              col_2 = talla_names_2018,
                              fill = 'zz_fill')
df_talla_names <- data.frame(
  '2023' = sort(cols$col_1),
  '2018' = sort(cols$col_2)
)
missing_cols <- setdiff(talla_names_2023, talla_names_2018)
df_talla_names$yes_23_no_18 <- c(missing_cols, rep(NA, nrow(df_talla_names) - length(missing_cols)))
missing_cols <- setdiff(talla_names_2018, talla_names_2023)
df_talla_names$yes_18_no_23 <- c(missing_cols, rep(NA, nrow(df_talla_names) - length(missing_cols)))


# ---> Create dataframe with differences between capturas column names
cols <- make_cols_same_length(col_1 = capturas_names_2023,
                              col_2 = capturas_names_2018,
                              fill = 'zz_fill')

df_capturas_names <- data.frame(
  '2023' = sort(cols$col_1),
  '2018' = sort(cols$col_2)
)
missing_cols <- setdiff(capturas_names_2023, capturas_names_2018)
df_capturas_names$yes_23_no_18 <- c(missing_cols, rep(NA, nrow(df_capturas_names) - length(missing_cols)))
missing_cols <- setdiff(capturas_names_2018, capturas_names_2023)
df_capturas_names$yes_18_no_23 <- c(missing_cols, rep(NA, nrow(df_capturas_names) - length(missing_cols)))
write.xlsx(list(
  dif_columnas_tallas = df_talla_names,
  dif_columnas_capturas = df_capturas_names
), file = file.path(DATA_OUTPUT, 'db_differences', 'diferencia_columnas_april_2018_jun2023.xlsx'))

# (2) Make sure 2018 and 2023 have the same column names
to_delete_cols <- c("...4", "Expr1011", "SP_OBJ", "SP_OBJ2")
col_name_mapping <- list(
  "ARQUEO_TRB" = "ARQUEO TRB",
  "EN MUDA" = "MUDA",
  "Idjornada" = "idjornada",
  "rango" = "Rango"
)

# # (2) Rename columns
# col_name_mapping <- list(
#   'LON inicio' = 'start_long',
#   'LAT inicio' = 'start_lat',
#   'LON final' = 'end_long',
#   'LAT final' = 'end_lat'
# )
#
# old_columns <- names(col_name_mapping)
# new_columns <- unlist(unname(col_name_mapping))
# data.table::setnames(jun_2023_tallas_db, old = old_columns, new = new_columns)
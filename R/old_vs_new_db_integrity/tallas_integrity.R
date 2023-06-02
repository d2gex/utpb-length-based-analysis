source("old_vs_new_db_integrity/db_integrity.R")
output_path <- "../../data/sensitive/output/old_new_db_differences.xlsx"
if (!exists('old_db_tallas'))
  old_db_tallas <- read_csv2("../data/sensitive/consulta_utpb_2018/CONSULTA BDP_UTPB_TALLAS_16-05-2018.csv",
                             locale = locale(encoding = 'latin1'))

if (!exists('new_db_tallas'))
  new_db_tallas <- read_csv2("../data/sensitive/CONSULTA BDP_UTPB_TALLAS_17-04-2023_.csv",
                             locale = locale(encoding = 'latin1'))

integrity_data <- IntegrityData$new()
integrity_data$sought_columns <- c('Idlance', 'ESPECIE', 'PUERTO_EMBARQUE', 'Madurez',
                                   'NUMINDIVS', 'N TRIPUS', 'ARTE', 'Piezas', 'ZONA', 'OBSER1')
integrity_data$potential_diff_columns <- list(
  'TALLA' = c('Idlance', 'TALLA'),
  'PESO' = c('Idlance', 'PESO'),
  'OVADA' = c('Idlance', 'OVADA'),
  'Colorhuevos' = c('Idlance', 'Colorhuevos'),
  'CoD' = c('Idlance', 'CoD'),
  'HorafV' = c('Idlance', 'HorafV'),
  'HorafL' = c('Idlance', 'HorafL'),
  'FVIR' = c('Idlance', 'FVIR'),
  'FLARG' = c('Idlance', 'FLARG'),
  'mcarte1' = c('Idlance', 'mcarte1')
)
db_comparator <- DbVersionComparator$new(old_db_tallas, new_db_tallas, integrity_data, 1000)
result <- db_comparator$run()
# Get data that need to be reported because integrity issues

# ---> Get lances associated to columns which have same length
# but different values in the old vs new dataframes
lances_per_column <- get_lances_for_columns_with_different_values(result$id_lances_sample,
                                                                  integrity_data$potential_diff_columns,
                                                                  result$old_cut_df,
                                                                  result$new_df)
# --->  Add lances that have different number of rows for the old and new dataframe
if (length(result$hauls_with_different_length) > 0) {
  lances_per_column[['ESPECIE']] <- result$hauls_with_different_length
}
excel_data <- build_list_of_column_with_associated_data(lances_per_column,
                                                        result$old_cut_df,
                                                        result$new_df)


# -->  Now get the actual data for lances that are absent in the new dataframe
lances_per_column <- list(Piezas = result$absent_lances)
added_columns_data <- build_list_of_column_with_associated_data(lances_per_column,
                                                                old_db_tallas %>% select(Idlance, Piezas),
                                                                result$new_df)
excel_data[['Piezas']] <- added_columns_data[['Piezas']]
write.xlsx(excel_data, file = output_path)

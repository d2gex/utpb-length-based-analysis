# Install Microsoft Access 2013 runtime via
# https://learn.microsoft.com/en-us/office/troubleshoot/access/cannot-use-odbc-or-oledb to get the ODBC 64bit driver
# for Access, if such driver does not appear on the Administrative tools
library("RODBC")
library("logger")
source("utils.R")
source("db/db_context.R")
source("db/db_queries.R")

tallas_db <- "../../../../repos_data/utpblbm/data/sensitive/acess_db/Pesca-tablas.accdb"
db_context <- DbContext$new()
day <- 1
month <- 1
date_sep <- '/'
start_year <- 1999
end_year <- 2024
year_step <- 1
years_interval <- seq(from = start_year, to = end_year, by = year_step)
date_intervals <- build_yearly_date_intervals(day, month, date_sep, start_year, end_year, year_step)
tallas_df <- NULL
n_rows <- c()
db_connection <- odbcConnectAccess2007(tallas_db)
for (interval in date_intervals) {
  start <- 1
  end <- 2
  log_info(paste("Running query for interval", paste(interval, collapse = "-->")))
  query <- build_talla_query_interval(from_date = interval[start], to_date = interval[end], db_context)
  year_df <- sqlQuery(db_connection, query, stringsAsFactors = FALSE)
  if (is.null(tallas_df)) {
    tallas_df <- year_df
  }
  else {
    tallas_df <- rbind(tallas_df, year_df)
  }
  n_rows <- append(n_rows, nrow(year_df))
}
close(db_connection)

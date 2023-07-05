# Install Microsoft Access 2013 runtime via
# https://learn.microsoft.com/en-us/office/troubleshoot/access/cannot-use-odbc-or-oledb to get the ODBC 64bit driver
# for Access, if such driver does not appear on the Administrative tools
library("RODBC")
source("db/db_context.R")
source("db/tallas_query.R")

db_context <- DbContext$new()
tallas_db<- "../../../../repos_data/utpblbm/data/sensitive/acess_db/Pesca-tablas.accdb"
db_connection<-odbcConnectAccess2007(tallas_db)
tallas_df <- sqlQuery(db_connection,
                      build_talla_query_interval(from_date = '2/2/2000', to_date = '2/2/2001', db_context),
                      stringsAsFactors = FALSE)
close(db_connection)
#' ETL Configuration for MySQL Datamart Load
#'
#' This script defines all configuration parameters for the ETL pipeline,
#' including SQLite source settings, MySQL target credentials, date range,
#' and batch sizing.

etl.sqlite.file <- "expenses.sqlite"

etl.mysql.host     <- "mysql-practicum-practicum-dbms.a.aivencloud.com"
etl.mysql.port     <- 12187
etl.mysql.user     <- "avnadmin"
etl.mysql.password <- "AVNS_dc9v-KSDVuTItQzQNij"
etl.mysql.dbname   <- "practicum_db"

etl.date.start <- as.Date("2019-01-01")
etl.date.end   <- as.Date("2024-12-31")

etl.month.names <- month.name

etl.tables.drop <- c(
  "expense_facts", "dim_date", "dim_employee", "dim_client",
  "dim_project", "dim_category", "dim_currency", "dim_merchant"
)

etl.fact.batch.size <- 500

#' This script loads data from a CSV file into an SQLite database. It reads the 
#' CSV file, processes the data, and inserts it into the appropriate tables in 
#' the database. The script uses the RSQLite package to interact with the SQLite
#' database.
#' 
#' usage: Rscript loadToSQLite.R [filename]
#' @param filename The name of the CSV file to load into the SQLite database.
#' 
#' @author Adnesh Dhamangaonkar

source("constants.R")
source("dbconstants.R")

#' Loads data from the SQLite database into a list of data frames.
#' 
#' @param conn A database connection object.
loadDataFromSQLite <- function(conn) {
  data <- list()
  data$account <- dbGetQuery(conn, "SELECT ID, ACCOUNT_NUMBER, EMPLOYEE_ID FROM ACCOUNT;")
  data$card <- dbGetQuery(conn, "SELECT ID, CARD_TYPE FROM CARD;")
  data$currency <- dbGetQuery(conn, "SELECT ID, CURRENCY_CODE FROM CURRENCY;")
  data$subcategory <- dbGetQuery(conn, "SELECT ID, SUBCATEGORY_NAME FROM SUBCATEGORY;")
  data
}

#' Loads data from a CSV file into the SQLite database.
#' 
#' @param conn A database connection object.
loadDataToSQLite <- function(conn, data) {
  metaData <- loadDataFromSQLite(conn)
  csvData <- data
  transactions <- data.frame(
    ACCOUNT_ID = metaData$account$ID[
      match(data$Account.Number, metaData$account$ACCOUNT_NUMBER)
    ],
    DATE = as.Date(data$Date),
    MERCHANT = data$Description,
    AMOUNT = data$Amount,
    CARD_ID = metaData$card$ID[match(data$Card, metaData$card$CARD_TYPE)],
    CURRENCY_ID = metaData$currency$ID[
      match(data$Currency, metaData$currency$CURRENCY_CODE)
    ],
    SUBCATEGORY_ID = metaData$subcategory$ID[
      match(data$Subcategory, metaData$subcategory$SUBCATEGORY_NAME)
    ],
    BILLABLE = ifelse(data$Billable == "Y", 1, 0),
    stringsAsFactors = FALSE
  )
  batchInsert(conn, "USER_TRANSACTION", transactions)
}

addToProcessedFiles <- function(conn, filename) {
  cat("adding file", filename, "to PROCESSED_FILES table\n")
  dbExecute(
    conn,
    "INSERT INTO PROCESSED_FILES (FILE_NAME, PROCESSED_AT) VALUES (?, ?);",
    params = list(filename, Sys.time())
  )
}

checkIfProcessed <- function(conn, filename) {
  result <- dbGetQuery(
    conn,
    "SELECT COUNT(*) AS count FROM PROCESSED_FILES WHERE FILE_NAME = ?;",
    params = list(filename)
  )
  result$count[1] > 0
}

main <- function() {
  args = commandArgs(trailingOnly = TRUE)
  argLength = length(args)
  if(argLength != 1)
    cat("usage: Rscript loadToSQLite.R [filename]\n")
  else {
    conn <- getConnection()
    on.exit(dbDisconnect(conn))
    csvfile <- args[1]
    if(!file.exists(csvfile)) {
      cat("file", csvfile, "does not exist\n")
    } else if(checkIfProcessed(conn, csvfile)) {
      cat("file", csvfile, "has already been processed. skipping...\n")
    } else {
      csvData <- read.csv(csvfile, stringsAsFactors = FALSE)
      cat("loading csv data to SQLite database...\n")
      loadDataToSQLite(conn, csvData)
      addToProcessedFiles(conn, csvfile)
      cat("data loaded successfully!\n")
    }
  }
}

main()
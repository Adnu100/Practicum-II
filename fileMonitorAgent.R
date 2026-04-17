#' This file monitor continuously checks for CSV file and for each file found, 
#' it reads the file, processes the data and stores the data in SQLite database.
#' 
#' @author Adnesh Dhamangaonkar

source("dbconstants.R")

#' Continuously monitors the current directory for new CSV files and processes
#' them. This function checks for new CSV files at regular intervals (default
#' is every 5 seconds) and processes any new files found by calling the
#' loadToSQLite.R script. It keeps track of already processed files to avoid
#' reprocessing. After processing the file, it simulates creating expense 
#' report by invoking the script storeExpenseReport.R and generating a random
#' expense report.
#'
#' @param conn A database connection object to the SQLite database.
#' @param processedFiles A vector of file names that have already been processed.
#' @param pollInterval The interval (in seconds) at which to check for new files
watchFiles <- function(conn, processedFiles, pollInterval = 5) {
  cat("Starting file monitor...\n")
  while (TRUE) {
    csvFiles <- list.files(pattern = "\\.csv$")
    if(length(csvFiles) > 0)
      for(file in setdiff(csvFiles, processedFiles)) {
        cat("Processing file:", file, "\n")
        system2("Rscript", c("loadToSQLite.R", shQuote(file)))
        system2("Rscript", c("storeExpenseReport.R", strsplit(file, "-")[[1]][1]))
        processedFiles <- c(processedFiles, file)
      }
    Sys.sleep(pollInterval)
  }
}

main <- function() {
  conn <- sqlite.getConnection()
  on.exit(dbDisconnect(conn))
  alreadyProcessed <- dbGetQuery(conn, "SELECT FILE_NAME FROM PROCESSED_FILES;")
  watchFiles(conn, alreadyProcessed$FILE_NAME)
}

main()
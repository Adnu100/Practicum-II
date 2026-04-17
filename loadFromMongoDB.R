#' Author: Siddhi Satish Bhanushali
#' ETL: Load MongoDB Expense Report Allocations into MySQL Analytical Datamart
#'
#' This script reads expense reports stored in MongoDB by storeExpenseReport.R,
#' extracts the transaction allocations, and merges them into the expense_facts
#' table in MySQL. Each allocation maps a transaction to a specific project and
#' client with an allocated amount.
#'
#' usage: called from loadToMySql.R via loadMongoExpenseReports(mysql_con)

source("constants.R")
source("mongodbconstants.R")
source("loadToMySqlConfiguration.R")

install_if_missing("mongolite")
install_if_missing("RMySQL")

#' Build name-to-key lookups from dim_project and dim_client in MySQL.
#'
#' @param mysql_con A DBIConnection to MySQL.
#' @return A list with project_lookup and client_lookup named vectors.
buildDimLookups <- function(mysql_con) {
  projects <- dbGetQuery(mysql_con, "SELECT project_key, project_name FROM dim_project")
  clients  <- dbGetQuery(mysql_con, "SELECT client_key, client_name FROM dim_client")

  project_lookup <- projects$project_key
  names(project_lookup) <- tolower(trimws(projects$project_name))

  client_lookup <- clients$client_key
  names(client_lookup) <- tolower(trimws(clients$client_name))

  list(project_lookup = project_lookup, client_lookup = client_lookup)
}

#' Build a date string to date_key lookup from dim_date.
#'
#' @param mysql_con A DBIConnection to MySQL.
#' @return A named integer vector keyed by "YYYY-MM-DD".
buildDateLookup <- function(mysql_con) {
  dates <- dbGetQuery(mysql_con, "SELECT date_key, full_date FROM dim_date")
  date_lookup <- dates$date_key
  names(date_lookup) <- as.character(dates$full_date)
  date_lookup
}

#' Pull all expense reports from MongoDB and load allocations into expense_facts.
#'
#' MongoDB document structure (from mongolite):
#'   docs$employee          -> data.frame with columns: id, name
#'   docs$transactions      -> list of data.frames, one per report
#'     each row has: date, merchant, amount, card, category, subcategory, allocations
#'     allocations column   -> list of data.frames with columns: project, client, amount
#'
#' @param mysql_con A DBIConnection to MySQL.
loadMongoExpenseReports <- function(mysql_con) {
  cat("\nLoading expense report allocations from MongoDB...\n")

  mongo_con <- mongo.getConnection()
  on.exit(mongo_con$disconnect())

  total_docs <- mongo_con$count()
  cat(sprintf("Found %d expense reports in MongoDB\n", total_docs))
  if (total_docs == 0) {
    cat("No reports to load. Skipping.\n")
    return(invisible(NULL))
  }

  docs     <- mongo_con$find('{}')
  lookups  <- buildDimLookups(mysql_con)
  date_lkp <- buildDateLookup(mysql_con)

  fact_count    <- 0
  skipped_count <- 0

  dbBegin(mysql_con)
  tryCatch({
    for (r in seq_len(nrow(docs))) {

      emp_key <- as.integer(docs$employee[r, "id"])
      if (is.na(emp_key)) emp_key <- 0L

      txns <- docs$transactions[[r]]
      if (is.null(txns) || nrow(txns) == 0) next

      for (t in seq_len(nrow(txns))) {
        txn <- txns[t, ]

        dt  <- as.Date(as.character(txn$date))
        mo  <- as.integer(format(dt, "%m"))
        yr  <- as.integer(format(dt, "%Y"))
        dk  <- date_lkp[as.character(dt)]
        qtr <- ceiling(mo / 3L)

        if (is.na(dk)) {
          skipped_count <- skipped_count + 1
          next
        }

        txn_amt     <- as.numeric(txn$amount)
        alloc_df    <- txn$allocations[[1]]

        if (is.null(alloc_df) || nrow(alloc_df) == 0) {
          skipped_count <- skipped_count + 1
          next
        }

        total_alloc <- sum(as.numeric(alloc_df$amount))

        for (a in seq_len(nrow(alloc_df))) {
          proj_name <- tolower(trimws(as.character(alloc_df$project[a])))
          cli_name  <- tolower(trimws(as.character(alloc_df$client[a])))

          proj_key  <- lookups$project_lookup[proj_name]
          cli_key   <- lookups$client_lookup[cli_name]

          if (is.na(proj_key) || is.na(cli_key)) {
            skipped_count <- skipped_count + 1
            next
          }

          alloc_amt <- as.numeric(alloc_df$amount[a])
          alloc_pct <- if (total_alloc > 0) round(alloc_amt / total_alloc * 100, 2) else 100.00

          dbExecute(mysql_con, sprintf(
            "INSERT INTO expense_facts
               (date_key, employee_key, project_key, client_key, category_key,
                currency_key, merchant_key, txn_amount, billable,
                allocation_pct, allocated_amount, month, quarter, year)
             VALUES (%d, %d, %d, %d, 0, 0, 0, %.2f, 1, %.2f, %.2f, %d, %d, %d)",
            as.integer(dk), as.integer(emp_key),
            as.integer(proj_key), as.integer(cli_key),
            txn_amt, alloc_pct, alloc_amt, mo, qtr, yr
          ))
          fact_count <- fact_count + 1
        }
      }

      if (r %% 10 == 0) {
        dbCommit(mysql_con)
        dbBegin(mysql_con)
        cat(sprintf("Processed %d off %d reports\n", r, total_docs))
      }
    }

    dbCommit(mysql_con)
  }, error = function(e) {
    dbRollback(mysql_con)
    stop(paste("Error loading MongoDB reports:", e$message))
  })

  cat(sprintf("Loaded %d fact rows from MongoDB (%d skipped)\n", fact_count, skipped_count))
}
#' Constants and utility functions for database operations.
#' 
#' @author Adnesh Dhamangaonkar

source("constants.R")

install_if_missing("RSQLite")
install_if_missing("DBI")
install_if_missing("glue")

sqlite.file.name <- "transactions.db"

#' Establishes a connection to the SQLite database.
#'
#' @return A database connection object.
getConnection <- function() {
  conn <- dbConnect( RSQLite::SQLite(), sqlite.file.name)
  dbExecute(conn, "PRAGMA FOREIGN_KEYS = ON;")
  conn
}

#' Inserts data from a dataframe into the specified table in batches.
#'
#' @param conn A database connection object.
#' @param table The name of the table to insert data into.
#' @param df The dataframe containing the data to be inserted.
#' @param batch The number of rows to insert in each batch.
#' @param returnId A boolean indicating whether to return the inserted IDs.
#' @return A vector of inserted IDs if returnId is TRUE, otherwise NULL.
batchInsert <- function(conn, table, df, batch = 1000, returnId = FALSE) {
  cat("populating table", table, "\n")
  columnNames <- colnames(df)
  insertQuery <- sprintf(
    "INSERT INTO %s(%s) VALUES", table, paste(columnNames, collapse = ", ")
  )
  columnStr <- paste0("(", paste0("{", columnNames, "}", collapse = ", "), ")")
  if(returnId) {
    ids <-  list()
    index <- 1
  }
  totalRows <- 0
  withCallingHandlers(
    {
      dbBegin(conn)
      for (i in seq(1, nrow(df), by = batch)) {
        rows <- df[i:min(i + batch - 1, nrow(df)), ,drop = FALSE]
        values <- glue_data_sql(rows, columnStr, .con = conn, .na = SQL("NULL"))
        query <- paste0(insertQuery, paste(values, collapse = ","), ";")
        rows <- dbExecute(conn, paste0(query, ";"))
        totalRows <- totalRows + rows
        if(returnId) {
          insertId <- dbGetQuery(conn, "SELECT LAST_INSERT_ROWID()")[1, 1]
          ids[[index]] <- seq(from = insertId - rows + 1, length.out = rows)
          index <- index + 1
        }
      }
      dbCommit(conn)  
    },
    error = function(e) {
      dbRollback(conn)
      traceback()
      stop(e)
    }
  )
  cat(sprintf("inserted %d rows in table %s\n", totalRows, table))
  if(returnId)
    unlist(ids)
}

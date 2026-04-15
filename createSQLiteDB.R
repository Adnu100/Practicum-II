#' Create SQLite database and populate it with synthetic financial transaction
#' data
#' 
#' @author Adnesh Dhamangaonkar

source("constants.R")
source("dbconstants.R")
source("syntheticDataGeneratorConfiguration.R")

install_if_missing("charlatan")

install_if_missing("RSQLite")
install_if_missing("DBI")
install_if_missing("glue")

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

createSQLiteDB <- function(conn) {
  cat("creating SQLite database\n")
  tables <- c(
    "ACCOUNT", "EMPLOYEE", "CARD", "CURRENCY",
    "SUBCATEGORY", "CATEGORY", "USER_TRANSACTION"
  )
  for(query in paste("DROP TABLE IF EXISTS", tables, ";")) {
    dbExecute(conn, query)
  }
  
  dbExecute(conn, "
    CREATE TABLE EMPLOYEE (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      NAME TEXT
    );
  ")
  
  
  dbExecute(conn, "
    CREATE TABLE ACCOUNT (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      ACCOUNT_NUMBER TEXT,
      EMPLOYEE_ID INTEGER,
      FOREIGN KEY (EMPLOYEE_ID) REFERENCES EMPLOYEE(ID)
    );
  ")
  
  dbExecute(conn, "
    CREATE TABLE CARD (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      CARD_TYPE TEXT
    );
  ")
  
  dbExecute(conn, "
    CREATE TABLE CURRENCY (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      CURRENCY_CODE TEXT
    );
  ")
  
  dbExecute(conn, "
    CREATE TABLE SUBCATEGORY (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      SUBCATEGORY_NAME TEXT,
      CATEGORY_ID INTEGER,
      FOREIGN KEY (CATEGORY_ID) REFERENCES CATEGORY(ID)
    );
  ")
  
  dbExecute(conn, "
    CREATE TABLE CATEGORY (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      CATEGORY_NAME TEXT
    );
  ")
  
  dbExecute(conn, "
    CREATE TABLE USER_TRANSACTION (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      ACCOUNT_ID TEXT,
      DATE DATE,
      MERCHANT TEXT,
      AMOUNT REAL,
      CARD_ID INTEGER,
      CURRENCY_ID INTEGER,
      SUBCATEGORY_ID INTEGER,
      BILLABLE BOOLEAN,
      FOREIGN KEY(ACCOUNT_ID) REFERENCES ACCOUNT(ID),
      FOREIGN KEY (CARD_ID) REFERENCES CARD(ID),
      FOREIGN KEY (CURRENCY_ID) REFERENCES CURRENCY(ID),
      FOREIGN KEY (SUBCATEGORY_ID) REFERENCES SUBCATEGORY(ID)
    );
  ")
}

populateSQLiteDB <- function(conn) {
  cat("populating SQLite database\n")
  categoryIds <- batchInsert(
    conn,
    "CATEGORY",
    data.frame(CATEGORY_NAME = generator.transaction.categories),
    returnId = TRUE
  )
  cat("categoryIds :", categoryIds, "\n")
  subcategoryData <- do.call(
    rbind,
    lapply(
      generator.transaction.categories,
      function(category) {
        data.frame(
          SUBCATEGORY_NAME = generator.transaction.subcategories[[category]],
          CATEGORY_NAME = category
        )
      }
    )
  )
  batchInsert(
    conn,
    "SUBCATEGORY",
    data.frame(
      SUBCATEGORY_NAME = subcategoryData$SUBCATEGORY_NAME,
      CATEGORY_ID = categoryIds[
        match(subcategoryData$CATEGORY_NAME, generator.transaction.categories)
      ]
    )
  )
  batchInsert(conn, "CARD", data.frame(CARD_TYPE = generator.card.providers))
  batchInsert(conn, "CURRENCY", data.frame(CURRENCY_CODE = generator.currencies))
  employeeIds <- batchInsert(
    conn, "EMPLOYEE", data.frame(NAME = ch_name(n = 2000)), returnId = TRUE
  )
  globalEmployeeIds <<- employeeIds
  batchInsert(
    conn,
    "ACCOUNT",
    data.frame(
      ACCOUNT_NUMBER = generator.transaction.accounts,
      EMPLOYEE_ID = sample(
        employeeIds, length(generator.transaction.accounts), replace = TRUE
      )
    )
  )
}

getConnection <- function() {
  dbConnect( RSQLite::SQLite(), sqlite.file.name)
}

main <- function() {
  conn <- getConnection()
  on.exit(dbDisconnect(conn))
  createSQLiteDB(conn)
  populateSQLiteDB(conn)
  cat("SQLite database created and populated successfully\n")
}

main()
#' Create SQLite database and populate it with synthetic financial transaction
#' data
#' 
#' @author Adnesh Dhamangaonkar

source("dbconstants.R")
source("syntheticDataGeneratorConfiguration.R")

install_if_missing("charlatan")

#' Creates the SQLite database with the necessary tables and relationships.
#' 
#' @param conn A database connection object.
createSQLiteDB <- function(conn) {
  tables <- c(
    "USER_TRANSACTION", "ACCOUNT", "EMPLOYEE", "CARD", "CURRENCY",
    "SUBCATEGORY", "CATEGORY", "PROCESSED_FILES", "PROJECT_ASSIGNMENT",
    "PROJECT", "CLIENT"
  )
  for(query in paste("DROP TABLE IF EXISTS", tables, ";")) {
    cat("executing query:", query, "\n")
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
  
  dbExecute(conn, "
    CREATE TABLE PROCESSED_FILES (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      FILE_NAME TEXT,
      PROCESSED_AT DATETIME
    );
  ")
  
  dbExecute(
    conn, "CREATE INDEX PROCESSED_FILES_INDEX ON PROCESSED_FILES(FILE_NAME);"
  )
  
  dbExecute(conn, "
    CREATE TABLE PROJECT (
      ID INTEGER PRIMARY KEY,
      CLIENT_ID INTEGER,
      PROJECT_NAME TEXT,
      FOREIGN KEY (CLIENT_ID) REFERENCES CLIENT(ID)
    );
  ")
  
  dbExecute(conn, "
    CREATE TABLE CLIENT (
      ID INTEGER PRIMARY KEY,
      CLIENT_NAME TEXT
    );
  ")
  
  dbExecute(conn, "
    CREATE TABLE PROJECT_ASSIGNMENT (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      EMPLOYEE_ID INTEGER,
      PROJECT_ID INTEGER,
      FOREIGN KEY (EMPLOYEE_ID) REFERENCES EMPLOYEE(ID),
      FOREIGN KEY (PROJECT_ID) REFERENCES PROJECT(ID)
    );
  ")
}

populateCategoryAndSubcategoryData <- function(conn) {
  categoryIds <- batchInsert(
    conn,
    "CATEGORY",
    data.frame(CATEGORY_NAME = generator.transaction.categories),
    returnId = TRUE
  )
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
}

populateCardAndCurrencyData <- function(conn, employeeIds) {
  batchInsert(conn, "CARD", data.frame(CARD_TYPE = generator.card.providers))
  batchInsert(
    conn, "CURRENCY", data.frame(CURRENCY_CODE = generator.currencies)
  )
}

populateRandomEmployees <- function(conn) {
  batchInsert(
    conn,
    "EMPLOYEE",
    data.frame(NAME = ch_name(n = generator.employees)),
    returnId = TRUE
  )
}

populateRandomEmployeeAccountProjectAssignments <- function(conn, employeeIds) {
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
  batchInsert(
    conn,
    "CLIENT",
    data.frame(
      ID = 1:generator.clients,
      CLIENT_NAME = replicate(generator.clients, ch_company())
    )
  )
  batchInsert(
    conn,
    "PROJECT",
    data.frame(
      ID = 1:generator.projects,
      PROJECT_NAME = paste("Project", 1:generator.projects),
      CLIENT_ID = sample(
        1:generator.clients, generator.projects, replace = TRUE
      )
    )
  )
  batchInsert(
    conn,
    "PROJECT_ASSIGNMENT",
    do.call(
      rbind,
      lapply(
        employeeIds,
        function(empId) {
          data.frame(
            EMPLOYEE_ID = empId,
            PROJECT_ID = sample(
              1:generator.projects, sample(1:5, 1), replace = FALSE
            )
          )
        }
      )
    )
  )
}

#' Populates the SQLite database with synthetic data for categories,
#' subcategories, cards, currencies, employees, and accounts.
#'
#' @param conn A database connection object.
populateSQLiteDB <- function(conn) {
  populateCategoryAndSubcategoryData(conn)
  populateCardAndCurrencyData(conn)
  employeeIds <- populateRandomEmployees(conn)
  populateRandomEmployeeAccountProjectAssignments(conn, employeeIds)
}

main <- function() {
  conn <- getConnection()
  on.exit(dbDisconnect(conn))
  cat("creating SQLite database\n")
  createSQLiteDB(conn)
  cat("populating SQLite database\n")
  populateSQLiteDB(conn)
  cat("SQLite database created and populated successfully\n")
}

main()
#' Author : Siddhi Satish Bhanushali
#' ETL: Load SQLite Expense Data into MySQL Analytical Datamart
#'
#' This script extracts data from a local SQLite database, transforms
#' it into a star schema (dimensions + fact table), and loads it into the MySQL
#' analytical datamart hosted on Aiven.
#' usage: Rscript loadToMySql.R

source("constants.R")
source("loadToMySqlConfiguration.R")

install_if_missing("RSQLite")
install_if_missing("RMySQL")

#' Connect to the SQLite source database.
#'
#' @return A DBIConnection to the SQLite database.
connectSQLite <- function() {
  con <- dbConnect(RSQLite::SQLite(), etl.sqlite.file)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  cat("Connected to SQLite.\n")
  con
}

#' Connect to the MySQL datamart on Aiven.
#'
#' @return A DBIConnection to the MySQL database.
connectMySQL <- function() {
  con <- dbConnect(
    RMySQL::MySQL(),
    host     = etl.mysql.host,
    port     = etl.mysql.port,
    user     = etl.mysql.user,
    password = etl.mysql.password,
    dbname   = etl.mysql.dbname
  )
  dbExecute(con, "SET sql_mode = 'ANSI_QUOTES'")
  cat("Connected to MySQL.\n")
  con
}

#' Drop all datamart tables in a order that tables without dependencies are deleted first.
#'
#' @param conn A DBIConnection to MySQL.
dropTables <- function(conn) {
  cat("Dropping existing tables...\n")
  for (tbl in etl.tables.drop) {
    tryCatch(
      dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tbl)),
      error = function(e) cat(sprintf("Could not drop %s: %s\n", tbl, e$message))
    )
  }
}

#' Create all dimension tables in the MySQL datamart.
#'
#' @param conn A DBIConnection to MySQL.
createDimensionTables <- function(conn) {
  cat("Creating dimension tables...\n")
  
  dbExecute(conn, "
    CREATE TABLE dim_date (
      date_key     INT         NOT NULL PRIMARY KEY,
      full_date    DATE        NOT NULL,
      day_of_month INT         NOT NULL,
      month        INT         NOT NULL,
      month_name   VARCHAR(20) NOT NULL,
      quarter      INT         NOT NULL,
      year         INT         NOT NULL,
      day_of_week  INT         NOT NULL,
      INDEX idx_date_month   (year, month),
      INDEX idx_date_quarter (year, quarter)
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE dim_employee (
      employee_key INT          NOT NULL PRIMARY KEY,
      first_name   VARCHAR(100) NOT NULL,
      last_name    VARCHAR(100) NOT NULL,
      email        VARCHAR(200),
      department   VARCHAR(100),
      INDEX idx_emp_name (last_name, first_name)
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE dim_client (
      client_key  INT          NOT NULL PRIMARY KEY,
      client_name VARCHAR(200) NOT NULL,
      country     VARCHAR(100),
      INDEX idx_client_name (client_name)
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE dim_project (
      project_key  INT          NOT NULL PRIMARY KEY,
      project_name VARCHAR(200) NOT NULL,
      client_key   INT          NOT NULL,
      budget       DECIMAL(12,2),
      start_date   DATE,
      end_date     DATE,
      INDEX idx_proj_client (client_key)
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE dim_category (
      category_key     INT          NOT NULL PRIMARY KEY AUTO_INCREMENT,
      category_name    VARCHAR(100) NOT NULL,
      subcategory_name VARCHAR(100) NOT NULL,
      subcategory_id   INT          NOT NULL,
      INDEX idx_cat_name    (category_name),
      INDEX idx_subcat_name (subcategory_name)
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE dim_currency (
      currency_key  INT         NOT NULL PRIMARY KEY,
      currency_code VARCHAR(10) NOT NULL,
      INDEX idx_currency_code (currency_code)
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE dim_merchant (
      merchant_key  INT          NOT NULL PRIMARY KEY,
      merchant_name VARCHAR(200) NOT NULL,
      INDEX idx_merchant_name (merchant_name)
    )
  ")
  
  cat("Dimension tables created.\n")
}

#' Create the expense_facts table with year-based range partitioning.
#'
#' @param conn A DBIConnection to MySQL.
createFactTable <- function(conn) {
  cat("Creating fact table...\n")
  
  dbExecute(conn, "
    CREATE TABLE expense_facts (
      fact_id          INT           NOT NULL AUTO_INCREMENT,
      date_key         INT           NOT NULL,
      employee_key     INT           NOT NULL,
      project_key      INT           NOT NULL,
      client_key       INT           NOT NULL,
      category_key     INT           NOT NULL,
      currency_key     INT           NOT NULL,
      merchant_key     INT           NOT NULL,
      txn_amount       DECIMAL(12,2) NOT NULL,
      billable         TINYINT       NOT NULL DEFAULT 0,
      allocation_pct   DECIMAL(5,2)  NOT NULL DEFAULT 100.00,
      allocated_amount DECIMAL(12,2) NOT NULL,
      month            INT           NOT NULL,
      quarter          INT           NOT NULL,
      year             INT           NOT NULL,
      PRIMARY KEY (fact_id, year),
      INDEX idx_fact_date               (date_key),
      INDEX idx_fact_employee           (employee_key),
      INDEX idx_fact_project            (project_key),
      INDEX idx_fact_client             (client_key),
      INDEX idx_fact_category           (category_key),
      INDEX idx_fact_period             (year, quarter, month),
      INDEX idx_fact_billable           (billable),
      INDEX idx_fact_client_proj_period (client_key, project_key, year, quarter, month)
    )
    PARTITION BY RANGE (year) (
      PARTITION p2019 VALUES LESS THAN (2020),
      PARTITION p2020 VALUES LESS THAN (2021),
      PARTITION p2021 VALUES LESS THAN (2022),
      PARTITION p2022 VALUES LESS THAN (2023),
      PARTITION p2023 VALUES LESS THAN (2024),
      PARTITION p2024 VALUES LESS THAN (2025),
      PARTITION pmax  VALUES LESS THAN MAXVALUE
    )
  ")
  
  cat("Fact table created.\n")
}

#' Insert all rows of a data frame into a MySQL table in batches.
#'
#' Builds multi-row INSERT statements for performance rather than
#' inserting one row at a time. Batch inserts rows.
#'
#' @param conn A DBIConnection to MySQL.
#' @param table The name of the target table (used in error messages).
#' @param df A data frame whose rows will be inserted.
#' @param sql_template A sprintf template: INSERT INTO tbl (cols) VALUES %s
#' @param batch_size Number of rows per INSERT statement.
mysqlBulkInsert <- function(conn, table, df, sql_template, batch_size = 500) {
  if (nrow(df) == 0) return(invisible(NULL))
  
  # Build a single VALUES row string from one row of df
  makeValuesRow <- function(row) {
    vals <- vapply(as.list(row), function(v) {
      if (is.na(v) || is.null(v)) "NULL"
      else if (is.numeric(v))     as.character(v)
      else                        paste0("'", gsub("'", "''", as.character(v)), "'")
    }, character(1))
    paste0("(", paste(vals, collapse = ", "), ")")
  }
  
  dbBegin(conn)
  tryCatch({
    for (start in seq(1, nrow(df), by = batch_size)) {
      end  <- min(start + batch_size - 1, nrow(df))
      rows <- df[start:end, , drop = FALSE]
      value_rows <- vapply(seq_len(nrow(rows)), function(i) makeValuesRow(rows[i, ]), character(1))
      sql <- sprintf(sql_template, paste(value_rows, collapse = ",\n"))
      dbExecute(conn, sql)
    }
    dbCommit(conn)
  }, error = function(e) {
    dbRollback(conn)
    stop(paste("Error inserting into", table, ":", e$message))
  })
}

#' Generate and load dim_date for the configured date range.
#'
#' Builds all date rows in R first, then inserts in batches for performance.
#'
#' @param conn A DBIConnection to MySQL.
loadDateDimension <- function(conn) {
  cat("  Generating date dimension...\n")
  all_dates <- seq(etl.date.start, etl.date.end, by = "day")
  dt_vec    <- as.Date(all_dates)
  mo_vec    <- as.integer(format(dt_vec, "%m"))
  yr_vec    <- as.integer(format(dt_vec, "%Y"))
  
  date_df <- data.frame(
    date_key     = as.integer(format(dt_vec, "%Y%m%d")),
    full_date    = as.character(dt_vec),
    day_of_month = as.integer(format(dt_vec, "%d")),
    month        = mo_vec,
    month_name   = etl.month.names[mo_vec],
    quarter      = ceiling(mo_vec / 3L),
    year         = yr_vec,
    day_of_week  = as.integer(format(dt_vec, "%u")),
    stringsAsFactors = FALSE
  )
  
  mysqlBulkInsert(
    conn, "dim_date", date_df,
    "INSERT INTO dim_date
       (date_key, full_date, day_of_month, month, month_name, quarter, year, day_of_week)
     VALUES %s"
  )
  cat(sprintf("Loaded %d dates\n", nrow(date_df)))
}

#' Load all dimension tables from SQLite into MySQL.
#'
#' @param src A DBIConnection to the SQLite source.
#' @param tgt A DBIConnection to the MySQL target.
loadDimensions <- function(src, tgt) {
  cat("\nLoading dimension data...\n")
  
  emp_raw    <- dbGetQuery(src, "SELECT ID, NAME FROM EMPLOYEE")
  name_parts <- strsplit(emp_raw$NAME, " ", fixed = TRUE)
  employees  <- data.frame(
    employee_key = emp_raw$ID,
    first_name   = sapply(name_parts, function(x) x[1]),
    last_name    = sapply(name_parts, function(x) if (length(x) > 1) paste(x[-1], collapse = " ") else ""),
    email        = NA_character_,
    department   = NA_character_,
    stringsAsFactors = FALSE
  )
  mysqlBulkInsert(
    tgt, "dim_employee", employees,
    "INSERT INTO dim_employee (employee_key, first_name, last_name, email, department) VALUES %s"
  )
  cat(sprintf("Loaded %d employees\n", nrow(employees)))
  
  clients <- dbGetQuery(src, "SELECT ID AS client_key, CLIENT_NAME AS client_name FROM CLIENT")
  clients$country <- NA_character_
  mysqlBulkInsert(
    tgt, "dim_client", clients,
    "INSERT INTO dim_client (client_key, client_name, country) VALUES %s"
  )
  cat(sprintf("Loaded %d clients\n", nrow(clients)))
  
  projects <- dbGetQuery(src, "SELECT ID AS project_key, PROJECT_NAME AS project_name, CLIENT_ID AS client_key FROM PROJECT")
  projects$budget     <- NA_real_
  projects$start_date <- NA_character_
  projects$end_date   <- NA_character_
  mysqlBulkInsert(
    tgt, "dim_project", projects,
    "INSERT INTO dim_project (project_key, project_name, client_key, budget, start_date, end_date) VALUES %s"
  )
  cat(sprintf("Loaded %d projects\n", nrow(projects)))
  
  cat_data <- dbGetQuery(src, "SELECT s.ID AS subcategory_id, c.CATEGORY_NAME AS category_name, s.SUBCATEGORY_NAME AS subcategory_name FROM SUBCATEGORY s JOIN CATEGORY c ON s.CATEGORY_ID = c.ID")
  for (i in seq_len(nrow(cat_data))) {
    dbExecute(tgt, sprintf(
      "INSERT INTO dim_category (category_key, category_name, subcategory_name, subcategory_id)
       VALUES (%d, '%s', '%s', %d)",
      cat_data$subcategory_id[i],
      gsub("'", "''", cat_data$category_name[i]),
      gsub("'", "''", cat_data$subcategory_name[i]),
      cat_data$subcategory_id[i]
    ))
  }
  cat(sprintf("Loaded %d categories/subcategories\n", nrow(cat_data)))
  
  currencies <- dbGetQuery(src, "SELECT ID AS currency_key, CURRENCY_CODE AS currency_code FROM CURRENCY")
  mysqlBulkInsert(
    tgt, "dim_currency", currencies,
    "INSERT INTO dim_currency (currency_key, currency_code) VALUES %s"
  )
  cat(sprintf("Loaded %d currencies\n", nrow(currencies)))
  
  merch_raw <- dbGetQuery(src,
                          "SELECT DISTINCT MERCHANT FROM USER_TRANSACTION WHERE MERCHANT IS NOT NULL ORDER BY MERCHANT"
  )
  merchants <- data.frame(
    merchant_key  = seq_len(nrow(merch_raw)),
    merchant_name = merch_raw$MERCHANT,
    stringsAsFactors = FALSE
  )
  mysqlBulkInsert(
    tgt, "dim_merchant", merchants,
    "INSERT INTO dim_merchant (merchant_key, merchant_name) VALUES %s"
  )
  cat(sprintf("Loaded %d merchants\n", nrow(merchants)))
  
  loadDateDimension(tgt)
}

#' Build a lookup list mapping each employee_id to their assigned projects and clients.
#'
#' Uses PROJECT_ASSIGNMENT joined to PROJECT to resolve the client for each project.
#' Employees with no assignment fall back to project_key = 0 / client_key = 0 in loadFacts.
#'
#' @param src A DBIConnection to the SQLite source.
#' @return A named list keyed by employee_id, each entry a list of project_id/client_id pairs.
buildAssignmentLookup <- function(src) {
  assignments <- dbGetQuery(src, "
    SELECT pa.EMPLOYEE_ID AS employee_id,
           pa.PROJECT_ID  AS project_id,
           p.CLIENT_ID    AS client_id
      FROM PROJECT_ASSIGNMENT pa
      JOIN PROJECT p ON pa.PROJECT_ID = p.ID
  ")
  lookup <- list()
  for (i in seq_len(nrow(assignments))) {
    eid            <- as.character(assignments$employee_id[i])
    n              <- length(lookup[[eid]]) + 1
    lookup[[eid]][[n]] <- list(
      project_id = assignments$project_id[i],
      client_id  = assignments$client_id[i]
    )
  }
  cat(sprintf("Built assignment lookup for %d employees\n", length(lookup)))
  lookup
}

#' Load the expense_facts table, allocating each transaction to its merchant key.
#'
#' Employees with no project assignment are loaded with project_key = 0 and
#' client_key = 0. A merchant lookup is built from dim_merchant in MySQL so that
#' each transaction's MERCHANT text maps to the correct merchant_key.
#' Inserts are committed in batches of etl.fact.batch.size for performance.
#'
#' @param src A DBIConnection to the SQLite source.
#' @param tgt A DBIConnection to the MySQL target.
loadFacts <- function(src, tgt) {
  cat("\nLoading fact table...\n")
  
  txn_data <- dbGetQuery(src, "
    SELECT t.ID            AS txn_id,
           t.DATE          AS txn_date,
           t.AMOUNT        AS amount,
           t.BILLABLE      AS billable,
           t.ACCOUNT_ID    AS account_id,
           t.SUBCATEGORY_ID AS subcategory_id,
           t.CURRENCY_ID   AS currency_id,
           t.MERCHANT      AS merchant_name
      FROM USER_TRANSACTION t
  ")
  cat(sprintf("Processing %d transactions...\n", nrow(txn_data)))
  
  accounts <- dbGetQuery(src, "SELECT ID AS account_id, EMPLOYEE_ID AS employee_id FROM ACCOUNT")
  acc_lookup <- accounts$employee_id
  names(acc_lookup) <- as.character(accounts$account_id)
  
  merch_dim    <- dbGetQuery(tgt, "SELECT merchant_key, merchant_name FROM dim_merchant")
  merch_lookup <- merch_dim$merchant_key
  names(merch_lookup) <- merch_dim$merchant_name
  
  assign_lookup <- buildAssignmentLookup(src)
  
  dbBegin(tgt)
  fact_count <- 0
  
  for (i in seq_len(nrow(txn_data))) {
    row  <- txn_data[i, ]
    dt   <- as.Date(row$txn_date)
    mo   <- as.integer(format(dt, "%m"))
    yr   <- as.integer(format(dt, "%Y"))
    dk   <- as.integer(format(dt, "%Y%m%d"))
    qtr  <- ceiling(mo / 3L)
    
    emp_key <- acc_lookup[as.character(row$account_id)]
    if (is.na(emp_key)) emp_key <- 0L
    
    merch_key <- merch_lookup[row$merchant_name]
    if (is.na(merch_key)) merch_key <- 0L
    
    projs <- assign_lookup[[as.character(emp_key)]]
    
    if (is.null(projs) || length(projs) == 0) {
      dbExecute(tgt, sprintf(
        "INSERT INTO expense_facts
           (date_key, employee_key, project_key, client_key, category_key,
            currency_key, merchant_key, txn_amount, billable,
            allocation_pct, allocated_amount, month, quarter, year)
         VALUES (%d, %d, 0, 0, %d, %d, %d, %.2f, %d, 100.00, %.2f, %d, %d, %d)",
        dk, emp_key, row$subcategory_id, row$currency_id,
        merch_key, row$amount, row$billable,
        round(row$amount, 2), mo, qtr, yr
      ))
      fact_count <- fact_count + 1
    } else {
      n_proj    <- length(projs)
      alloc_pct <- round(100 / n_proj, 2)
      for (j in seq_along(projs)) {
        this_pct <- if (j == n_proj) 100 - alloc_pct * (n_proj - 1) else alloc_pct
        dbExecute(tgt, sprintf(
          "INSERT INTO expense_facts
             (date_key, employee_key, project_key, client_key, category_key,
              currency_key, merchant_key, txn_amount, billable,
              allocation_pct, allocated_amount, month, quarter, year)
           VALUES (%d, %d, %d, %d, %d, %d, %d, %.2f, %d, %.2f, %.2f, %d, %d, %d)",
          dk, emp_key, projs[[j]]$project_id, projs[[j]]$client_id,
          row$subcategory_id, row$currency_id, merch_key,
          row$amount, row$billable, this_pct,
          round(row$amount * this_pct / 100, 2), mo, qtr, yr
        ))
        fact_count <- fact_count + 1
      }
    }
    
    if (i %% etl.fact.batch.size == 0) {
      dbCommit(tgt)
      dbBegin(tgt)
      cat(sprintf("Processed %d of %d transactions\n", i, nrow(txn_data)))
    }
  }
  
  dbCommit(tgt)
  cat(sprintf("Loaded %d fact rows.\n", fact_count))
}

#' Print row counts for all datamart tables and a sample aggregation query.
#'
#' @param conn A DBIConnection to MySQL.
verifyDatamart <- function(conn) {
  cat("\nMySQL datamart row counts:\n")
  for (tbl in c("dim_date", "dim_employee", "dim_client", "dim_project",
                "dim_category", "dim_currency", "dim_merchant", "expense_facts")) {
    cnt <- dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s", tbl))
    cat(sprintf("%s: %d rows\n", tbl, cnt$n))
  }
  
  cat("\nSample - total expenses by category per quarter:\n")
  print(dbGetQuery(conn, "
    SELECT dc.category_name,
           ef.year, ef.quarter,
           ROUND(SUM(ef.allocated_amount), 2) AS total_expense,
           ROUND(AVG(ef.allocated_amount), 2) AS avg_expense,
           COUNT(*) AS num_transactions
      FROM expense_facts ef
      JOIN dim_category dc ON ef.category_key = dc.category_key
     GROUP BY dc.category_name, ef.year, ef.quarter
     ORDER BY ef.year, ef.quarter, dc.category_name
     LIMIT 15
  "))
}

main <- function() {
  source("loadFromMongoDB.R")
  
  sqlite_con <- connectSQLite()
  mysql_con  <- connectMySQL()
  
  dropTables(mysql_con)
  createDimensionTables(mysql_con)
  createFactTable(mysql_con)
  loadDimensions(sqlite_con, mysql_con)
  loadFacts(sqlite_con, mysql_con)
  loadMongoExpenseReports(mysql_con)
  verifyDatamart(mysql_con)
  
  dbDisconnect(sqlite_con)
  dbDisconnect(mysql_con)
  cat("\nAll connections closed. ETL complete.\n")
}

main()
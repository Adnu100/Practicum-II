#' Author : Siddhi Satish Bhanushali
#' ETL: Load SQLite Expense Data into MySQL Analytical Datamart
#'
#' This script extracts operational data from a local SQLite database, transforms
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
  con <- dbConnect(SQLite(), etl.sqlite.file)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  cat("Connected to SQLite.\n")
  con
}

#' Connect to the MySQL analytical datamart on Aiven.
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

#' Drop all datamart tables in reverse dependency order.
#'
#' @param conn A DBIConnection to MySQL.
dropTables <- function(conn) {
  cat("Dropping existing tables...\n")
  for (tbl in etl.tables.drop) {
    tryCatch(
      dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tbl)),
      error = function(e) cat(sprintf("  Note: could not drop %s: %s\n", tbl, e$message))
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

#' Insert all rows of a data frame into a MySQL table.
#'
#' @param conn A DBIConnection to MySQL.
#' @param table The name of the target table (used in error messages).
#' @param df A data frame whose rows will be inserted.
#' @param sql_template A sprintf template with a single %s placeholder for the VALUES clause.
mysqlBulkInsert <- function(conn, table, df, sql_template) {
  if (nrow(df) == 0) return(invisible(NULL))

  dbBegin(conn)
  tryCatch({
    for (i in seq_len(nrow(df))) {
      vals <- vapply(as.list(df[i, ]), function(v) {
        if (is.na(v) || is.null(v)) "NULL"
        else if (is.numeric(v))     as.character(v)
        else                        paste0("'", gsub("'", "''", as.character(v)), "'")
      }, character(1))
      dbExecute(conn, sprintf(sql_template, paste(vals, collapse = ", ")))
    }
    dbCommit(conn)
  }, error = function(e) {
    dbRollback(conn)
    stop(paste("Error inserting into", table, ":", e$message))
  })
}

#' Generate and load dim_date for the configured date range.
#'
#' @param conn A DBIConnection to MySQL.
loadDateDimension <- function(conn) {
  cat("  Generating date dimension...\n")
  all_dates <- seq(etl.date.start, etl.date.end, by = "day")

  dbBegin(conn)
  for (d in as.character(all_dates)) {
    dt <- as.Date(d)
    mo <- as.integer(format(dt, "%m"))
    yr <- as.integer(format(dt, "%Y"))
    dbExecute(conn, sprintf(
      "INSERT INTO dim_date
         (date_key, full_date, day_of_month, month, month_name, quarter, year, day_of_week)
       VALUES (%d, '%s', %d, %d, '%s', %d, %d, %d)",
      as.integer(format(dt, "%Y%m%d")), d,
      as.integer(format(dt, "%d")), mo, etl.month.names[mo],
      ceiling(mo / 3), yr, as.integer(format(dt, "%u"))
    ))
  }
  dbCommit(conn)
  cat(sprintf("  Loaded %d dates\n", length(all_dates)))
}

#' Load all dimension tables from SQLite into MySQL.
#'
#' @param src A DBIConnection to the SQLite source.
#' @param tgt A DBIConnection to the MySQL target.
loadDimensions <- function(src, tgt) {
  cat("\nLoading dimension data...\n")

  employees <- dbGetQuery(src, "SELECT * FROM employees")
  mysqlBulkInsert(tgt, "dim_employee", employees,
    "INSERT INTO dim_employee (employee_key, first_name, last_name, email, department) VALUES (%s)")
  cat(sprintf("  Loaded %d employees\n", nrow(employees)))

  clients <- dbGetQuery(src, "SELECT * FROM clients")
  mysqlBulkInsert(tgt, "dim_client", clients,
    "INSERT INTO dim_client (client_key, client_name, country) VALUES (%s)")
  cat(sprintf("  Loaded %d clients\n", nrow(clients)))

  projects <- dbGetQuery(src, "SELECT * FROM projects")
  mysqlBulkInsert(tgt, "dim_project", projects,
    "INSERT INTO dim_project (project_key, project_name, client_key, budget, start_date, end_date) VALUES (%s)")
  cat(sprintf("  Loaded %d projects\n", nrow(projects)))

  cat_data <- dbGetQuery(src, "
    SELECT s.subcategory_id, c.category_name, s.subcategory_name
      FROM subcategories s
      JOIN categories c ON s.category_id = c.category_id
  ")
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
  cat(sprintf("  Loaded %d categories/subcategories\n", nrow(cat_data)))

  currencies <- dbGetQuery(src, "SELECT * FROM currencies")
  mysqlBulkInsert(tgt, "dim_currency", currencies,
    "INSERT INTO dim_currency (currency_key, currency_code) VALUES (%s)")
  cat(sprintf("  Loaded %d currencies\n", nrow(currencies)))

  merchants <- dbGetQuery(src, "SELECT * FROM merchants")
  mysqlBulkInsert(tgt, "dim_merchant", merchants,
    "INSERT INTO dim_merchant (merchant_key, merchant_name) VALUES (%s)")
  cat(sprintf("  Loaded %d merchants\n", nrow(merchants)))

  loadDateDimension(tgt)
}

#' Build a lookup list mapping each employee_id to their assigned projects and clients.
#'
#' @param assignments A data frame with columns employee_id, project_id, client_id.
#' @return A named list keyed by character employee_id, each value a list of
#'         lists with fields project_id and client_id.
buildAssignmentLookup <- function(assignments) {
  lookup <- list()
  for (i in seq_len(nrow(assignments))) {
    eid            <- as.character(assignments$employee_id[i])
    n              <- length(lookup[[eid]]) + 1
    lookup[[eid]][[n]] <- list(
      project_id = assignments$project_id[i],
      client_id  = assignments$client_id[i]
    )
  }
  lookup
}

#' Load the expense_facts table, allocating each transaction evenly across projects.
#'
#' Employees with no project assignment are loaded with project_key = 0 and client_key = 0.
#' Inserts are committed in batches of etl.fact.batch.size for performance.
#'
#' @param src A DBIConnection to the SQLite source.
#' @param tgt A DBIConnection to the MySQL target.
loadFacts <- function(src, tgt) {
  cat("\nLoading fact table...\n")

  txn_data <- dbGetQuery(src, "
    SELECT t.txn_id, t.txn_date, t.amount, t.billable,
           t.employee_id, t.subcategory_id, t.currency_id, t.merchant_id
      FROM transactions t
  ")

  assign_lookup <- buildAssignmentLookup(dbGetQuery(src, "
    SELECT a.employee_id, a.project_id, p.client_id
      FROM assignments a
      JOIN projects p ON a.project_id = p.project_id
  "))

  cat(sprintf("  Processing %d transactions...\n", nrow(txn_data)))

  dbBegin(tgt)
  fact_count <- 0

  for (i in seq_len(nrow(txn_data))) {
    row   <- txn_data[i, ]
    dt    <- as.Date(row$txn_date)
    mo    <- as.integer(format(dt, "%m"))
    yr    <- as.integer(format(dt, "%Y"))
    dk    <- as.integer(format(dt, "%Y%m%d"))
    qtr   <- ceiling(mo / 3)
    projs <- assign_lookup[[as.character(row$employee_id)]]

    if (is.null(projs) || length(projs) == 0) {
      dbExecute(tgt, sprintf(
        "INSERT INTO expense_facts
           (date_key, employee_key, project_key, client_key, category_key,
            currency_key, merchant_key, txn_amount, billable,
            allocation_pct, allocated_amount, month, quarter, year)
         VALUES (%d, %d, 0, 0, %d, %d, %d, %.2f, %d, 100.00, %.2f, %d, %d, %d)",
        dk, row$employee_id, row$subcategory_id, row$currency_id,
        row$merchant_id, row$amount, row$billable,
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
          dk, row$employee_id, projs[[j]]$project_id, projs[[j]]$client_id,
          row$subcategory_id, row$currency_id, row$merchant_id,
          row$amount, row$billable, this_pct,
          round(row$amount * this_pct / 100, 2), mo, qtr, yr
        ))
        fact_count <- fact_count + 1
      }
    }

    if (i %% etl.fact.batch.size == 0) {
      dbCommit(tgt)
      dbBegin(tgt)
      cat(sprintf("    Processed %d / %d transactions\n", i, nrow(txn_data)))
    }
  }

  dbCommit(tgt)
  cat(sprintf("  Loaded %d fact rows.\n", fact_count))
}

#' Print row counts for all datamart tables and a sample aggregation query.
#'
#' @param conn A DBIConnection to MySQL.
verifyDatamart <- function(conn) {
  cat("\nMySQL datamart row counts:\n")
  for (tbl in c("dim_date", "dim_employee", "dim_client", "dim_project",
                "dim_category", "dim_currency", "dim_merchant", "expense_facts")) {
    cnt <- dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s", tbl))
    cat(sprintf("  %s: %d rows\n", tbl, cnt$n))
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
  sqlite_con <- connectSQLite()
  mysql_con  <- connectMySQL()

  dropTables(mysql_con)
  createDimensionTables(mysql_con)
  createFactTable(mysql_con)
  loadDimensions(sqlite_con, mysql_con)
  loadFacts(sqlite_con, mysql_con)
  verifyDatamart(mysql_con)

  dbDisconnect(sqlite_con)
  dbDisconnect(mysql_con)
  cat("\nAll connections closed. ETL complete.\n")
}

main()

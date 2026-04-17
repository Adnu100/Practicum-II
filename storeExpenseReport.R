#' Script for storing the expense report in MongoDB. 
#' 
#' This script takes an account number as command line argument and then creates an
#' expense report by creating random allocations for randomly selected transactions
#' from the account and stores the expense report in MongoDB.
#' 
#' usage: Rscript storeExpenseReport.R [accountNumber]
#' @param accountNumber the account number for which the expense report is to be generated.
#'
#' @author Adnesh Dhamangaonkar

source("syntheticDataGeneratorConfiguration.R")
source("mongodbconstants.R")
source("dbconstants.R")

install_if_missing("jsonlite")

unboxRecursive <- function(x) {
  if (is.list(x))
    lapply(x, unboxRecursive)
  else if(is.atomic(x) && length(x) == 1)
    unbox(x)
  else x
}

#' Creates an expense report with the given employee, approver, transactions and allocations.
#' This report includes the employee and approver information, the date of report creation,
#' the status of the report, the total amount of the report, and a list of transactions with
#' their respective allocations. This can be stored in mongoDB for later retrieval and analysis.
#' 
#' @param employee A data frame containing the employee's ID and name.
#' @param approver A data frame containing the approver's ID and name.
#' @param transactions A data frame containing the transactions to be included in the report.
#' @param allocations A list of allocations corresponding to each transaction,
#'                    where each allocation is a list of projects and amounts allocated to
#'                    those projects.
#' @return A list representing the expense report
createExpenseReport <- function(employee, approver, transactions, allocations) {
  unboxRecursive(list(
    employee = list(id = employee$ID, name = employee$NAME),
    approver = list(id = approver$ID, name = approver$NAME),
    reportDate = Sys.Date(),
    status = "Submitted",
    totalAmount = sum(transactions$AMOUNT),
    transactions = lapply(
      seq.int(from = 1, to = nrow(transactions)),
      function(i) {
        transaction <- transactions[i, ]
        list(
          date = transaction$DATE,
          merchant = transaction$MERCHANT,
          amount = transaction$AMOUNT,
          card = transaction$CARD_TYPE,
          category = transaction$CATEGORY_NAME,
          currency = transaction$CURRENCY_CODE,
          subcategory = transaction$SUBCATEGORY_NAME,
          allocations = allocations[[i]]
        )
      }
    )  
  ))
}

#' Retrieves the employee's ID and name from the database based on the provided account number.
#' This function executes a SQL query to fetch the employee's information and returns it as a
#' data frame.
#'
#' @param conn A database connection object.
#' @param accountNumber The account number associated with the employee whose
#'                      information is to be retrieved.
#' @return A data frame containing the employee's ID and name.
getEmployeeIdAndName <- function(conn, accountNumber) {
  dbGetQuery(
    conn,
    "
    SELECT EMPLOYEE.ID, EMPLOYEE.NAME
    FROM EMPLOYEE INNER JOIN ACCOUNT ON EMPLOYEE.ID = ACCOUNT.EMPLOYEE_ID
    WHERE ACCOUNT_NUMBER = ?;
    ",
    params = list(accountNumber)
  )
}

#' Retrieves the employee's information, including their assigned projects and billable
#' transactions, from the database based on the provided employee ID. This function executes
#' SQL queries to fetch the employee's basic information, their assigned projects along with
#' client names, and their billable transactions. The transactions are filtered to include only
#' those that are billable and have a negative amount (indicating expenses). The function
#' returns a list containing the employee's information, their projects, and their transactions.
#' 
#' @param conn A database connection object.
#' @param accountNumber The account number associated with the employee whose
#'                      information is to be retrieved.
#' @return A list containing the employee's information, their assigned projects, and their
#'         billable transactions.
getEmployeInformation <- function(conn, accountNumber) {
  employee <- getEmployeeIdAndName(conn, accountNumber)
  employeeId <- employee$ID[1]
  projects <- dbGetQuery(
    conn,
    "
    SELECT p.PROJECT_NAME, c.CLIENT_NAME
    FROM PROJECT p
    INNER JOIN PROJECT_ASSIGNMENT pa ON p.ID = pa.PROJECT_ID
    INNER JOIN CLIENT c ON p.CLIENT_ID = c.ID
    WHERE pa.EMPLOYEE_ID = ?;
    ",
    params = list(employeeId)
  )
  transactions <- dbGetQuery(
    conn,
    "
    SELECT
      t.DATE, t.MERCHANT, t.AMOUNT, cu.CURRENCY_CODE, cd.CARD_TYPE, c.CATEGORY_NAME, s.SUBCATEGORY_NAME
    FROM USER_TRANSACTION t
    INNER JOIN ACCOUNT a ON t.ACCOUNT_ID = a.ID
    INNER JOIN SUBCATEGORY s ON t.SUBCATEGORY_ID = s.ID
    INNER JOIN CATEGORY c ON s.CATEGORY_ID = c.ID
    INNER JOIN CURRENCY cu ON t.CURRENCY_ID = cu.ID
    INNER JOIN CARD cd ON t.CARD_ID = cd.ID
    WHERE a.EMPLOYEE_ID = ? AND t.BILLABLE = 1 AND t.AMOUNT < 0;
    ",
    params = list(employeeId)
  )
  transactions$AMOUNT <- abs(transactions$AMOUNT)
  list(employee = employee, projects = projects, transactions = transactions)
}

#' Generates a random allocation of the transaction amount across the employee's assigned projects.
#' This function takes a transaction and a list of projects as input and randomly allocates The
#' transaction amount across the projects. The allocation is done in such a way that the total
#' allocated amount equals the transaction amount. The function returns a list of allocations,
#' where each allocation includes the project name, client name, and the allocated amount for that
#' project. Allocations with an amount of zero are filtered out from the final result.
#'
#' @param transaction A data frame representing a single transaction, including its amount.
#' @param projects A data frame containing the employee's assigned projects and their respective
#'        client names.
#' @return A list of allocations for the transaction, where each allocation includes the
#'         project name, client name, and allocated amount.
generateRandomAllocationForTransaction <- function(transaction, projects) {
  projects <- projects[sample(nrow(projects), sample(1:nrow(projects), 1)), ]
  numProjects <- nrow(projects)
  allocation <- list()
  remainingAmount <- transaction$AMOUNT
  for(i in seq_len(numProjects)) {
    if(i == numProjects) {
      allocation[[i]] <- list(
        project = projects$PROJECT_NAME[i],
        client = projects$CLIENT_NAME[i],
        amount = remainingAmount
      )
    } else {
      allocatedAmount <- round(runif(1, min = 0, max = remainingAmount), 2)
      allocation[[i]] <- list(
        project = projects$PROJECT_NAME[i],
        client = projects$CLIENT_NAME[i],
        amount = allocatedAmount
      )
      remainingAmount <- remainingAmount - allocatedAmount
    }
  }
  Filter(function(x) x$amount > 0, allocation)
}

#' Generates random allocations for all transactions of an employee. This function iterates over
#' each transaction and generates a random allocation for it using the
#' `generateRandomAllocationForTransaction` function. The result is a list of allocations
#' corresponding to each transaction, which can be used to create an expense report for the
#' employee. Each allocation in the list contains the project name, client name, and the
#' allocated amount for that transaction.
#'
#' @param transactions A data frame containing the employee's transactions.
#' @param projects A data frame containing the employee's assigned projects and their respective
#'        client names.
#' @return A list of allocations for all transactions, where each allocation corresponds to a
#'         transaction and includes the project name, client name, and allocated amount.
generateRandomAllocationForAllTransactions <- function(transactions, projects) {
  lapply(seq_len(nrow(transactions)), function(i) {
    generateRandomAllocationForTransaction(transactions[i, ], projects)
  })
}

main <- function() {
  conn <- sqlite.getConnection()
  mongoconn <- mongo.getConnection()
  on.exit(dbDisconnect(conn))
  on.exit(mongoconn$disconnect())

  args <- commandArgs(trailingOnly = TRUE)
  if(length(args) != 1) {
    cat("usage: Rscript storeExpenseReport.R [accountNumber]\n")
  } else {
    accountNumber <- args[1]
    employeeInfo <- getEmployeInformation(conn, accountNumber)
    selectedTransactions <- employeeInfo$transactions[
      sample(nrow(employeeInfo$transactions), sample(5:50, 1), replace = FALSE),
    ]
    allocations <- generateRandomAllocationForAllTransactions(
      selectedTransactions, employeeInfo$projects
    )
    expenseReport <- createExpenseReport(
      employeeInfo$employee,
      getEmployeeIdAndName(conn, sample(generator.transaction.accounts, 1)),
      selectedTransactions,
      allocations
    )
    mongoConn <- mongo.getConnection()
    mongoConn$insert(expenseReport)
    cat("Expense report created for employee", employeeInfo$employee$NAME[1], "\n")
  }
}

main()

source("constants.R")
source("dbconstants.R")

loadDataFromSQLite <- function(conn) {
  data <- list()
  data$account <- dbGetQuery(conn, "SELECT ID, ACCOUNT_NUMBER, EMPLOYEE_ID FROM ACCOUNT;")
  data$card <- dbGetQuery(conn, "SELECT ID, CARD_TYPE FROM CARD;")
  data$currency <- dbGetQuery(conn, "SELECT ID, CURRENCY_CODE FROM CURRENCY;")
  data$subcategory <- dbGetQuery(conn, "SELECT ID, SUBCATEGORY_NAME FROM SUBCATEGORY;")
  data
}

loadDataToSQLite <- function(conn, data) {
  metaData <<- loadDataFromSQLite(conn)
  csvData <<- data
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

main <- function() {
  args = commandArgs(trailingOnly = TRUE)
  argLength = length(args)
  if(argLength != 1)
    cat("usage: Rscript loadToSQLite.R [filename]\n")
  else {
    conn <- getConnection()
    on.exit(dbDisconnect(conn))
    csvData <- read.csv(args[1], stringsAsFactors = FALSE)
    cat("loading csv data to SQLite database...\n")
    loadDataToSQLite(conn, csvData)
    cat("data loaded successfully!\n")
  }
}

main()
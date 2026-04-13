source("constants.R")
source("syntheticDataGeneratorConfiguration.R")

install_if_missing("charlatan")

createSyntheticData <- function(transactions) {
  descriptions <- vapply(seq_len(transactions), function(i) {
    toupper(ch_company())
  }, character(1))
  
  dates <- sample(
    seq(from = generator.date.start, to = generator.date.end, by = "day"),
    transactions,
    replace = TRUE
  )
  
  accountNumber <- sample(
    replicate(
      generator.transaction.accounts,
      paste0(sample(1:9, 10, replace = TRUE), collapse = "")
    ),
    transactions,
    replace = TRUE
  )
  
  card <- sample(generator.card.providers, transactions, replace = TRUE)
  
  categoryMap <- list(
    Travel  = c("Airfare", "Ground Transport", "Car Rental", "Tolls & Parking"),
    Lodging = c("Hotel", "Serviced Apartment"),
    Meals   = c("Restaurants", "Coffee & Snacks", "Room Service"),
    Office  = c("Supplies", "Printing", "Shipping"),
    Telecom = c("Mobile", "Internet"),
    Retail  = c("Electronics", "General Merchandise"),
    Fuel    = c("Gasoline", "EV Charging")
  )
  
  categories  <- names(categoryMap)
  category    <- sample(
    generator.transaction.categories, transactions, replace = TRUE
  )
  subcategory <- vapply(
    category,
    function(cat) {
      sample(generator.transaction.subcategories[[cat]], 1)
    },
    character(1)
  )
  
  currency <- sample(
    generator.currencies,
    transactions,
    replace = TRUE,
    prob = generator.currencies.probabilities
  )
  
  billable <- sample(
    c("Y", "N"),
    transactions,
    replace = TRUE,
    prob = c(
      generator.transaction.billable.probability,
      1 - generator.transaction.billable.probability
    )
  )
  
  amount <- round(
    runif(
      transactions,
      min = generator.transaction.amount.min, 
      max = generator.transaction.amount.max
    ), 2
  )
  signs <- sample(
    c(1, -1),
    transactions,
    replace = TRUE,
    prob = c(
      generator.transaction.credit.probability,
      1 - generator.transaction.credit.probability
    )
  )
  
  data.frame(
    transaction_date = format(dates, "%m/%d/%Y"),
    description = descriptions,
    amount = amount * signs,
    credit_card = card,
    category = category,
    subcategory = subcategory,
    currency = currency,
    billable = billable,
    account_number = accountNumber,
    stringsAsFactors = FALSE
  )
}

createSyntheticDataCsv <- function(transactions, filename = "transactions.csv") {
  transactions <- createSyntheticData(transactions)
  transactions <- transactions[order(transactions$transaction_date), ]
  write.csv(transactions, file = filename, row.names = FALSE, quote = TRUE)
}

main <- function() {
  args = commandArgs(trailingOnly = TRUE)
  argLength = length(args)
  if(argLength > 2)
    cat("usage: Rscript syntheticDatqGenerator.R [number of rows]\n")
  else {
    rows = ifelse(argLength == 2, as.numeric(args[2]), 500)
    filename = ifelse(argLength >= 1, args[1], "transactions.csv")
    createSyntheticDataCsv(rows, filename)
    if(file.exists(filename)) {
      cat("warning: file already exists; overwriting...\n")
    }
    cat(sprintf("Wrote %d synthetic transactions to %s\n", rows, filename))
  }
}

main()

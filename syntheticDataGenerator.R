#' Synthetic Data Generator for Financial Transactions
#'
#' This script generates synthetic financial transaction data and saves it as a CSV file.
#' The generated data includes various fields such as date, merchant, amount, card provider,
#' category, subcategory, currency, billable status, and account number.
#' usage: Rscript syntheticDataGenerator.R [filename] [number of rows]
#' @param filename The name of the CSV file to create (default is "transactions.csv").
#' @param number of rows The number of transactions to generate (default is 500).

source("constants.R")
source("syntheticDataGeneratorConfiguration.R")

install_if_missing("charlatan")

#' Generate a binary probability vector for a given probability.
#' 
#' @param prob1 The probability of the first outcome (e.g., credit).
#' @return A vector containing the probabilities of both outcomes (e.g., credit and debit).
binaryProbabilityVector <- function(prob1) {
  c(prob1, 1 - prob1)
}

#' Create a random generator function that samples from a given set of values.
#'
#' @param total The number of samples to generate.
#' @return A function that takes a vector of values and an optional probability vector, and
#'         returns a sample of the specified size.
randomGenerator <- function(total) {
  function(x, prob = NULL) {
    sample(x, total, replace = TRUE, prob = prob)
  }
}

#' Generate synthetic financial transaction data.
#'
#' @param transactions The number of transactions to generate.
#' @return A data frame containing the generated transaction data.
createSyntheticData <- function(transactions) {
  random <- randomGenerator(transactions)
  category <- random(generator.transaction.categories)
  subcategory <- vapply(
    category,
    function(cat) {
      sample(generator.transaction.subcategories[[cat]], 1)
    },
    character(1)
  )
  amount <- round(
    runif(
      transactions,
      min = generator.transaction.amount.min, 
      max = generator.transaction.amount.max
    ), 2
  )
  signs <- random(
    c(1, -1),
    prob = binaryProbabilityVector(generator.transaction.credit.probability)
  )
  setNames(
    data.frame(
      random(seq(generator.date.start, generator.date.end, by = "day")),
      replicate(transactions, ch_company()),
      amount * signs,
      random(generator.card.providers),
      category,
      subcategory,
      random(generator.currencies, generator.currencies.probabilities),
      random(
        c("Y", "N"),
        prob = binaryProbabilityVector(generator.transaction.billable.probability)
      ),
      random(
        replicate(
          generator.transaction.accounts,
          paste0(sample(1:9, 10, replace = TRUE), collapse = "")
        )
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fix.empty.names = FALSE
    ),
    generator.csv.field.names
  )
}

#' Create a CSV file with synthetic financial transaction data.
#'
#' @param rows The number of transactions to generate.
#' @param filename The name of the CSV file to create (default is "transactions.csv").
createSyntheticDataCsv <- function(rows, filename = "transactions.csv") {
  transactions <- createSyntheticData(rows)
  transactions <- transactions[order(transactions$Date), ]
  write.csv(transactions, file = filename, row.names = FALSE, quote = TRUE)
}

main <- function() {
  args = commandArgs(trailingOnly = TRUE)
  argLength = length(args)
  if(argLength > 2)
    cat("usage: Rscript syntheticDatqGenerator.R [filename] [number of rows]\n")
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

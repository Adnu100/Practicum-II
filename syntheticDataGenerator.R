source("constants.R")
source("syntheticDataGeneratorConfiguration.R")

install_if_missing("charlatan")

binaryProbabilityVector <- function(prob1) {
  c(prob1, 1 - prob1)
}

randomGenerator <- function(total) {
  function(x, prob = NULL) {
    sample(x, total, replace = TRUE, prob = prob)
  }
}

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

createSyntheticDataCsv <- function(rows, filename = "transactions.csv") {
  transactions <- createSyntheticData(rows)
  transactions <- transactions[order(transactions$Date), ]
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

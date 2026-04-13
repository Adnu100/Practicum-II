source("constants.R")

install_if_missing("charlatan")

set.seed(42)

n_transactions <- 500

fake <- fraudster()

descriptions <- vapply(seq_len(n_transactions), function(i) {
  toupper(fake$company())
}, character(1))

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-12-31")

date_sequence <- seq(from = start_date, to = end_date, by = "day")

raw_dates <- sample(date_sequence, size = n_transactions, replace = TRUE)

txn_dates <- as.Date(as.POSIXct(raw_dates))

unique_accounts <- replicate(12, fake$phone_number())
unique_accounts <- paste0("ACCT-", gsub("[^0-9]", "", unique_accounts))
unique_accounts <- substr(unique_accounts, 1, 13)
unique_accounts <- unique(unique_accounts)

account_number <- sample(unique_accounts, n_transactions, replace = TRUE)

card_merchants <- c(
  "American Express", "Citi Platinum", "Chase Sapphire",
  "Capital One Venture", "Bank of America Premium"
)
credit_card <- sample(card_merchants, n_transactions, replace = TRUE)

category_map <- list(
  Travel  = c("Airfare", "Ground Transport", "Car Rental", "Tolls & Parking"),
  Lodging = c("Hotel", "Serviced Apartment"),
  Meals   = c("Restaurants", "Coffee & Snacks", "Room Service"),
  Office  = c("Supplies", "Printing", "Shipping"),
  Telecom = c("Mobile", "Internet"),
  Retail  = c("Electronics", "General Merchandise"),
  Fuel    = c("Gasoline", "EV Charging")
)

categories  <- names(category_map)
category    <- sample(categories, n_transactions, replace = TRUE)
subcategory <- vapply(category, function(cat) {
  sample(category_map[[cat]], 1)
}, character(1))

currencies   <- c("US$", "BRL", "Euro", "GBP")
currency_wts <- c(0.55, 0.20, 0.15, 0.10)
currency     <- sample(
  currencies, n_transactions, replace = TRUE, prob = currency_wts
)

billable <- sample(
  c("Y", "N"), n_transactions, replace = TRUE, prob = c(0.75, 0.25)
)

amount <- round(as.numeric(
  ch_double(n = n_transactions, mean = 400, sd = 350)), 2
)
amount <- ifelse(
  amount < 5, round(runif(length(amount[amount < 5]), 5, 50), 2), amount
)

credit_idx <- sample(
  seq_len(n_transactions), size = round(0.08 * n_transactions)
)
amount <--abs(amount)
amount[credit_idx] <- abs(amount[credit_idx])

transactions <- data.frame(
  transaction_date = format(txn_dates, "%m/%d/%Y"),
  description      = descriptions,
  amount           = amount,
  credit_card      = credit_card,
  category         = category,
  subcategory      = subcategory,
  currency         = currency,
  billable         = billable,
  account_number   = account_number,
  stringsAsFactors = FALSE
)

transactions <- transactions[order(txn_dates), ]
rownames(transactions) <- NULL

output_path <- "synthetic_transactions.csv"
write.csv(transactions, file = output_path, row.names = FALSE, quote = TRUE)

cat(sprintf("Wrote %d transactions to %s\n", nrow(transactions), output_path))
cat(sprintf("Date range: %s to %s\n", min(txn_dates), max(txn_dates)))
cat(sprintf("Unique accounts: %d\n", length(unique(account_number))))
cat(sprintf("Credit (refund) transactions: %d\n", length(credit_idx)))
cat("\nFirst 5 rows:\n")
print(head(transactions, 5))

#' Synthetic Data Generator Configuration
#' 
#' This script defines the configuration parameters for the synthetic data generator,
#' including date range, currencies, transaction details, and card providers.
#' 
#' @author Adnesh Dhamangaonkar

set.seed(123)

generator.date.start <- as.Date("2020-01-01")
generator.date.end <- as.Date("2023-12-31")

generator.currencies <- c("USD", "BRL", "Euro", "GBP")
generator.currencies.probabilities <- c(0.55, 0.20, 0.15, 0.10)

generator.transaction.billable.probability <- 0.75
generator.transaction.amount.min <- 2
generator.transaction.amount.max <- 20000
generator.transaction.credit.probability <- 0.1
generator.transaction.accounts <- replicate(
  5000, paste0(sample(1:9, 12, replace = TRUE), collapse = "")
)
generator.transaction.subcategories <- list(
  Travel  = c("Airfare", "Ground Transport", "Car Rental", "Tolls & Parking"),
  Lodging = c("Hotel", "Serviced Apartment"),
  Meals   = c("Restaurants", "Coffee & Snacks", "Room Service"),
  Office  = c("Supplies", "Printing", "Shipping"),
  Telecom = c("Mobile", "Internet"),
  Retail  = c("Electronics", "General Merchandise"),
  Fuel    = c("Gasoline", "EV Charging")
)
generator.transaction.categories = names(generator.transaction.subcategories)

generator.card.providers <- c(
  "American Express", "Citi Platinum", "Chase Sapphire",
  "Capital One Venture", "Bank of America Premium", "Visa", "Mastercard"
)

generator.csv.field.names <- c(
  "Date", "Account Number ", "Description", "Amount", "Card", 
  "Category", "Subcategory", "Currency", "Billable"
)

set.seed(NULL)
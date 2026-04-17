#' Constants for MongoDB connection and collection names
#' 
#' @author Adnesh Dhamangaonkar

source("constants.R")

install_if_missing("mongolite")

mongodb.connection.url <- "mongodb+srv://adnu:adnuStores@cluster0.gqht7un.mongodb.net/?appName=Cluster0"
mongodb.database.name <- "ExpenseReport"
mongodb.collection.name <- "reports"

mongo.getConnection <- function() {
  mongo(
    collection = mongodb.collection.name,
    db = mongodb.database.name,
    url = mongodb.connection.url
  )
}

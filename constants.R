#' Installs the specified package if it is not already installed and loads it.
#'
#' @param packageName The name of the package to install and load.
install_if_missing <- function(packageName) {
  if (!require(packageName, character.only = TRUE)) {
    install.packages(packageName, dependencies = TRUE)
    library(packageName, character.only = TRUE)
  }
}
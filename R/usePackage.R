usePackage <- function(package) {
  if (!is.element(package, installed.packages()[,1]))
    install.packages(package, dep = TRUE)
  library(package, character.only = TRUE)
}

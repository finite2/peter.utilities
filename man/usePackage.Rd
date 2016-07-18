\name{usePackage}
\alias{usePackage}
\docType{package}
\title{usePackage}
\description{
Will attempt to install the package specified from your default repository and then load it using the library command.
}
\usage{
usePackage(package)
}
\arguments{
\item{package}{Character string of the package you wish to load}
}
\examples{
## Maybe install and then load the binom package
usePackage("binom")
}
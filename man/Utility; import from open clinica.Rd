\name{import.from.open.clinica}
\alias{import.from.open.clinica}
\alias{import.open.cinical.batch}
\docType{package}
\title{import.from.open.clinica}
\description{
This function is the first step to importing data from open clinica to R.
It imports the comma delimited text file, formats dates, deals with labels and unneeded variables before saving in an r data format.
}
\usage{
import.open.cinical.batch(path,trial.stub,new.folder,dates=NULL)
import.from.open.clinica(file,path,new.folder,dates=NULL)
}
\arguments{
\item{file}{the file-name to convert to r format}
\item{path}{the location of the file to import}
\item{new.folder}{The location the file should be saved}
\item{dates}{A vector of dates to convert to posixct date-times (note columns with date in the title are converted automatically)}
}
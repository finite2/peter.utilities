\name{updateSlots}
\alias{updateSlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update an s4object from a data.frame
}
\description{
	Takes row in the data.frame simsToRun and converts this into the required s4object to run the simulation. For coding simplicity it is required that you provide an example of the s4 class object, this will be overwritten in the procedure.
}
\usage{
updateSlots(rowID, simsToRun, s4object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rowID}{
	The row index to take the parameters from
}
  \item{simsToRun}{
	The data.frame of simulation parameters
}
  \item{s4object}{
	an initialised s4object of the required class. This will be overwritten with the required parameters for the simulation
}
}

\details{
Note there are three columns to be ignored, the first is a rowID, the second last is an idicator for if a simulation has been completed and the last is a filepath for the results of the simulation.
}

\value{
Returns the updated s4object. this will have the same class as the s4object provided as input
}

\author{
Peter Dutton
}

\seealso{
\code{\link{addrow}}, \code{\link{getValue}}
}

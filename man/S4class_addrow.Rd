\name{addrow}
\alias{addrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
addrow to simulation list
}
\description{
Takes an s4 class and assigns the values to the next row of a data.frame for storing simulation runs. If there are nested s4 classes these are added using slot1_slot2 where you would get these slots normally using  s4object@slot1@slot2.
}
\usage{
addrow(s4object, simsToRun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s4object}{
	The s4 class object to append to the data.frame
}
  \item{simsToRun}{
	The data.frame of simulation parameters
}
}
\value{
Returns a data.frame with an additional row.
}

\author{
Peter Dutton
}
\seealso{
\code{\link{updateSlots}}, \code{\link{getValue}}
}


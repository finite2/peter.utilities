\name{getValue}
\alias{getValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get value from s4object
}
\description{
Obtain value from and S4object. This value may be nested up to two levels deep
}
\usage{
getValue(txtVar, s4object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txtVar}{
	character string of the slot object to return. If this is two levels deep then replace @ with _.
}
  \item{s4object}{
	s4object to get the value from
}
}

\value{
Returns the value of the slot for the s4object
}
\author{
Peter Dutton
}
\seealso{
\code{\link{updateSlots}}, \code{\link{addrow}}
}
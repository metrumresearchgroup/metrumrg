\name{locf}
\alias{forbak}
\alias{bakfor}
\alias{locf}
\alias{nocb}
\title{Impute Missing Vector Values}
\description{
	Impute \code{NA} values using other values in the vector.
}
\usage{
locf(x)
forbak(x)
bakfor(x)
nocb(x)
}
\arguments{
  \item{x}{A vector with some missing values and some non-missing values.}
}
\details{
\code{locf} implements \dQuote{last observation carried forward}:  \code{NA}'s are imputed with 
the most recent non-\code{NA} value.  \code{nocb} is the complement: \dQuote{next observation 
carried backward}:  \code{NA}'s are imputed with the next non-\code{NA} value.  \code{forbak}
first executes \code{locf}, then \code{nocb}, so that even leading \code{NA}s are imputed. 
If even one non-\code{NA} value is present, \code{forbak} should not return any \code{NA}s. \code{bakfor}
does the reverse.
}
\value{A vector of the same class as \code{x}.}
\author{Tim Bergsma}
\examples{
locf(c(NA,1,2,NA,NA,3,NA,4,NA))
nocb(c(NA,1,2,NA,NA,3,NA,4,NA))
forbak(c(NA,1,2,NA,NA,3,NA,4,NA))
}
\keyword{manip}


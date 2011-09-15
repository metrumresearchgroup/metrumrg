\name{map}
\alias{map}
\title{Map One Set of Values to Another}
\description{
Systematically substitute specific values from one set using corresponding values from 
another set.
}
\usage{
map(x, from, to)
}
\arguments{
  \item{x}{vector}
  \item{from}{vector (elements usually unique)}
  \item{to}{vector with same length as \code{from}}
}
\details{
Occasionally one wants to recode a set of categories using some other idiom.
\code{factor} supports recoding by creative use of the arguments \code{levels}
and \code{labels}.  However, the result is a factor, and may need more
transformation.  Furthermore, \code{factor} allows one-to-one reclassification
but not many-to-one reclassification (repeated levels is not supported; i.e., it
is not directly possible to collapse two levels to a single replacement code).

Here, \code{from} is the discrete set of values we expect in \code{x}, and 
\code{to} is the element-wise corresponding values with which we wish to re-present
elements in \code{x}.  Values in \code{x} not found in \code{from} will be
represented as NA (which is itself a legitimate value for \code{to}).  It is an
error if \code{from} and \code{to} have different lengths.

}
\value{
	vector of same class as \code{to} and same length as \code{x}
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{match}}
	}
}
\examples{
map(
	c('white','Asian','Indian'),
	from=c('white','White','black','Black','asian','Asian'),
	to=  c(   1,      1,      2,      2,      3,      3   )
)
}
\keyword{manip}


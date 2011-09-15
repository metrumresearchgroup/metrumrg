\name{safe.call}
\alias{safe.call}
\title{Call a Function Safely}
\description{
Some functions do not accept extra arguments.  \code{safe.call} passes only those
 arguments that will be recognized.
}
\usage{
safe.call(what, ...)
}
\arguments{
  \item{what}{a function}
  \item{\dots}{extra arguments, to be filtered}
}
\value{the result of the called function}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
	}
}
\keyword{manip}


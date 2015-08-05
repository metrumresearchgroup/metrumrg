\name{monotonic}
\alias{monotonic}
\alias{monotonic.default}
\alias{nondecreasing}
\alias{nondecreasing.default}
\alias{nonincreasing}
\alias{nonincreasing.default}

\title{Determine Whether a Vector is Monotonic}
\description{
	If each successive value in a numeric vector is less-than-or-equal-to
	the previous, or if each successive value is greater-than-or-equal-to
	the previous, we understand the vector as a whole to be \code{monotonic}.
	The functions \code{nondecreasing} and \code{nonincreasing} give element-wise
	diagnoses, showing positions at which the assertion holds (element 1 is 
	always TRUE). Use \code{strict=TRUE} for strictly less-than or greater-than
	logic.
}
\usage{
 \method{nondecreasing}{default}(x, strict=FALSE, ...)
 \method{nonincreasing}{default}(x, strict=FALSE, ...)
 \method{monotonic}{default}(x, strict=FALSE, ...)

}

\arguments{
  \item{x}{vector}
  \item{strict}{if FALSE, repeated values are tolerated. See examples.}
  \item{\dots}{ignored}
}

\value{logical.  A single valueFor \code{monotonic}; 
A vector (same length as x) \code{nondecreasing} and \code{nonincreasing}.
}
\references{
\url{http://metrumrg.googlecode.com}
}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{prev}}
	}
}
\examples{
x <- c(1,2,2,3,4)
nondecreasing(x)
nonincreasing(x)
monotonic(x)
monotonic(x,strict=TRUE)
}
\keyword{manip}


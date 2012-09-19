\name{pool}
\alias{pool}
\title{Test Set Relations Among Two Vectors}
\description{Test set relations among two vectors.}
 \usage{
pool(x,y)
}
\arguments{
  \item{x}{vector}
  \item{y}{vector}
}
\details{
	The two vectors are tested for elements unique to x, unique to y, and common
	to both.
}
\value{
list with elements x, y, and both
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{setdiff}}
		\item \code{\link{intersect}}
	}
}
\examples{
pool(
	letters[1:5],
	letters[4:8]
)
}
\keyword{manip}

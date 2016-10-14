\name{convertLines}
\alias{convertLines}
\title{Convert Line Endings}
\description{
Convert line endings to local encoding, by default.
}
\usage{
convertLines(
	source,
	destination=source,
	...
)
}
\arguments{
  \item{source}{file path}
  \item{destination}{file path}
  \item{\dots}{passed to \code{readLines} and \code{writeLines}}
}
\details{
By default, the source file is overwritten with lines terminating in the local line ending.
}
\value{
Used for side effects.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{readLines}}
		\item \code{\link{writeLines}}
	}
}
\keyword{manip}


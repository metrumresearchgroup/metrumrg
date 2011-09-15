\name{isSubversioned}
\alias{isSubversioned}
\title{Check Whether Files Are Subversioned}
\description{Check whether multiple files under Subversion control.}
\usage{isSubversioned(x, ...)
}
\arguments{
  \item{x}{character vector of  file names}
  \item{\dots}{ignored arguments.}
}
\details{This is a vectorized version of \code{isSubversionedFile}.}
\value{logical}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{isSubversionedFile}}
	}
}
\keyword{manip}

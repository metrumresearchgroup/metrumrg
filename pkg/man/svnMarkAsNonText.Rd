\name{svnMarkAsNonText}
\alias{svnMarkAsNonText}
\title{Mark Subversioned Files as Non-text.}
\description{Mark Subversioned files as non-text.}
\usage{svnMarkAsNonText(x, ...)
}
\arguments{
  \item{x}{character vector of file names}
  \item{\dots}{ignored}
}
\details{
For each Subversioned file in \code{x}, the property \sQuote{svn:mime-type}
is set to \sQuote{application/octet-stream}.
}
\value{used for side effects}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{isSubversioned}}
		\item \code{\link{svnIsText}}
		\item \code{\link{svnMarkAsText}}
		\item \code{\link{svnPropSet}}
	}
}
\keyword{manip}

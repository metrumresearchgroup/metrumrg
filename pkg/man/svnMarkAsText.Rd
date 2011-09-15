\name{svnMarkAsText}
\alias{svnMarkAsText}
\title{Mark Subversioned Files as Text}
\description{Mark subversioned files as text, rather than binary.}
\usage{svnMarkAsText(x, ...)
}
\arguments{
  \item{x}{character vector of file names}
  \item{\dots}{ignored}
}
\details{
For each Subversioned file in \code{x}, the property \sQuote{svn:mime-type}
is set to \sQuote{text/}.
}
\value{used for side effects}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{isSubversioned}}
		\item \code{\link{svnIsText}}
		\item \code{\link{svnMarkAsNonText}}
		\item \code{\link{svnPropSet}}
	}
}
\keyword{manip}

\name{svnMimeType}
\alias{svnMimeType}
\title{Determine Mime Type for Multiple Files}
\description{Determine Subversion mime type for multiple files.}
\usage{svnMimeType(x, ...)
}
\arguments{
  \item{x}{character vector of file names}
  \item{\dots}{ignored}
}
\details{Checks the property \sQuote{svn:mime-type} for subversioned files. 
	NA where file is not subversioned.}
\value{character}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{

	\itemize{
		\item \code{\link{svnPropSet}}
	}
}
\keyword{manip}

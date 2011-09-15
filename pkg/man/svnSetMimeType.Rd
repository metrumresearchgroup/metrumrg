\name{svnSetMimeType}
\alias{svnSetMimeType}
\title{Set Subversion Mime Type}
\description{Set Subversion mime type for multiple files.}
\usage{svnSetMimeType(x, type, ...)
}
\arguments{
  \item{x}{character vector}
  \item{type}{scalar character}
  \item{\dots}{ignored}
}
\details{
	Sets Subversion property \sQuote{svn:mime-type} to \code{type}, 
	e.g. \sQuote{/text} or \sQuote{application/octet-stream}.
	Just a wrapper for svnPropSet.
}
\value{used for side effects}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{svnPropSet}}
	}
}
\keyword{manip}


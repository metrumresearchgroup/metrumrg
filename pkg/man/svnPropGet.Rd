\name{svnPropGet}
\alias{svnPropGet}
\title{Get Subversion Property for a Vector of Files}
\description{Retrieve a given Subversion property for multiple files.}
\usage{svnPropGet(x,prop,...)}
\arguments{
  \item{x}{character vector of file names}
  \item{prop}{a property}
  \item{\dots}{ignored arguments}
}
\details{	
	A vectorized verson of svnPropGetFile.
}
\value{	character}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{svnPropGetFile}}
	}
}
\keyword{manip}


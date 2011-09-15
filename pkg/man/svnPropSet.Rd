\name{svnPropSet}
\alias{svnPropSet}
\title{Set a Property on a Vector of Subversioned Files}
\description{Set a property on multiple files.}
\usage{svnPropSet(x, prop, value, ...)
}
\arguments{
  \item{x}{character vector of file names}
  \item{prop}{scalar character property name}
  \item{value}{scalar character property value}
  \item{\dots}{ignored arguments}
}
\details{This is a vectorized version of svnPropSetFile.}
\value{used for side effects}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{svnPropSetFile}}
	}
}
\keyword{manip}

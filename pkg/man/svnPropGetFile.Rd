\name{svnPropGetFile}
\alias{svnPropGetFile}
\title{Get Subversion Property for a Single File}
\description{Get a subversion property for a single file. }
\usage{svnPropGetFile(file, prop)
}
\arguments{
  \item{file}{scalar character file name}
  \item{prop}{scalar character property name}
}
\details{Returns NA if file is not subversioned.
 Returns an empty string if the property is not defined.}
\value{character}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{isSubversionedFile}}
	}
}
\keyword{manip}

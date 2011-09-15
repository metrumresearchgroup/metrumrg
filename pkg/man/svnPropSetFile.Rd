\name{svnPropSetFile}
\alias{svnPropSetFile}
\title{Set a Property on a Subversioned File}
\description{Set a property on a subversioned file.}
\usage{svnPropSetFile(file, prop, value)
}
\arguments{
  \item{file}{scalar character file name}
  \item{prop}{scalar character property name}
  \item{value}{scalar character property value}
}
\details{It is an error if the file is not subversioned.}
\value{used for side effects}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{isSubversionedFile}}
	}
}
\keyword{manip}

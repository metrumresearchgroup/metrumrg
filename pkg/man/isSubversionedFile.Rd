\name{isSubversionedFile}
\alias{isSubversionedFile}
\title{Check If Single file Is Subversioned}
\description{Check if a single file is under Subversion control.}
\usage{isSubversionedFile(file)
}
\arguments{
  \item{file}{scalar character file name}
}
\details{May return \code{TRUE} even if file does not exist.}
\value{logical}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{isSubversioned}}
	}
}
\keyword{manip}

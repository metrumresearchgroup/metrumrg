\name{filename}
\alias{filename}

\title{Construct a Canonical File Name}
\description{
 Helper function to build file and directory names from components.
}
\usage{
filename(dir, run = NULL, ext = NULL)
}

\arguments{
  \item{dir}{directory}
  \item{run}{run name}
  \item{ext}{file extension}
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{dataSynthesis}}
	}
}
\examples{
filename('dir','3','.ctl')
}
\keyword{manip}


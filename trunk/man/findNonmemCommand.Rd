\name{findNonmemCommand}
\alias{findNonmemCommand}
\title{Find Paths to Available Nonmem Commands}
\description{
	NONR takes the path to a NONMEM executable as its \code{command}
	argument.  Discovery can be an issue.  This command sets probable
	defaults and searches for exact paths.
}
\usage{
findNonmemCommand(under='/opt/NONMEM',executable='autolog.pl',...)
}
\arguments{
  \item{under}{a directory for NONMEM installations}
  \item{executable}{file name sought}
  \item{\dots}{passed to \code{dir}}
}
\details{Arguments are simply passed through to \code{dir}.}
\value{
  character
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{NONR}}
	}
}
\keyword{manip}


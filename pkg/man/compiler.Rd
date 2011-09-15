\name{compiler}
\alias{compiler}

\title{Extract the Compiler Name from an NMQual Configuration File}
\description{
   This function reads the configuration file and extracts the last path element of 
   the first space-delimited component of the \sQuote{nmtran} instruction, typically a
   compiler name.
}
\usage{
compiler(config, pathsep = '/', ...)
}

\arguments{
  \item{config}{path for a configuration file}
  \item{pathsep}{file system path separator}
  \item{\dots}{ignored}
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runCommand}}
	}
}
\examples{
\dontrun{compiler('../NONMEM/nm6osx1/test/config.xml'))}
}
\keyword{manip}


\name{nmVersion}
\alias{nmVersion}

\title{Extract the NONMEM Version from an NMQual Configuration File}
\description{
   This function reads the configuration file and extracts the version number
   from the 'nonmem' element.
}
\usage{
nmVersion(config, ...)
}

\arguments{
  \item{config}{path for a configuration file}
  \item{\dots}{ignored}
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runNonmem}}
	}
}
\examples{
	\dontrun{nmVersion('../NONMEM/nm6osx1/test/config.xml'))}
}


\keyword{manip}


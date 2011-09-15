\name{config}
\alias{config}

\title{Identify the Configuration File in an NMQual-mediated NONMEM installation.}
\description{
   Gives the path for the configuration file in a NONMEM installation directory.
}
\usage{
config(dir, ...)
}
\arguments{
  \item{dir}{directory in which to find \file{config.xml}}
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
config('/common/nonmem/nm6')
}
\keyword{manip}


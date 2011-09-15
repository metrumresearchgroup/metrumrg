\name{episcript}
\alias{episcript}

\title{Run an Epilog Script in a Pre-populated Environment}
\description{
 Sources the named script in an environment where all other 
 passed arguments (...) have been defined by assignment.
}
\usage{
episcript(script, ...)
}

\arguments{
  \item{script}{path for an R script}
  \item{\dots}{variables with which to populate the script's environment}
}
\value{used for side effects}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runNonmem}}
	}
}
\keyword{manip}


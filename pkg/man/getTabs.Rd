\name{getTabs}
\alias{getTabs}

\title{Return the Observation Rows (EVID==0) Corresponding to the Specified Filename}
\description{
 Useful for returning the PK portion of NONMEM run output.
}
\usage{
getTabs(file)
}

\arguments{
	\item{file}{path for a file, typically \file{*.TAB}}
}
\value{data.frame}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
	}
}
\keyword{manip}


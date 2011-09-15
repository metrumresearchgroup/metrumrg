\name{getCwres}
\alias{getCwres}

\title{Return the Conditional Weighted Residuals Pertinent to a Given Run}
\description{
	Locates \file{cwtab1.deriv} and calls \code{compute.cwres}.  Then returns the CWRES column
	from \file{cwtab1}.
}
\usage{
getCwres(directory)
}

\arguments{
  \item{directory}{NONMEM run directory}
}
\value{numeric}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
	}
}
\keyword{manip}


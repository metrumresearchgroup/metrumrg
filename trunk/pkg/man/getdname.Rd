\name{getdname}
\alias{getdname}
\alias{getdname.default}
\alias{getdname.nmctl}

\title{Extract the Path and File from the DATA Block in the Control Stream Associated with a Filename}
\description{
	Extracts the data path from the control stream, e.g., for use with \code{getCovs}.
	Default method assumes a scalar character value and interprets it as a file name.
	It is an error if the file does not exist.  The main work is done by the 
	control stream method.
}
\usage{
	\method{getdname}{default}(x,...)
	\method{getdname}{nmctl}(x,...)
}
\arguments{
  \item{x}{object for dispatch}
  \item{\dots}{ignored}
}
\value{character}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
	}
}
\keyword{manip}


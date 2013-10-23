\name{specfile}
\alias{specfile}

\title{
Determine the Expected Path to a Specification File
}
\description{If one supplies a run name and project directory, the related control
stream can be sought, and the data file extracted.  One expects that the name
of any corresponding specification file has the suffix '.spec' instead of '.csv'.}
\usage{
specfile(
	run, 
	project = getwd(), 
	rundir = filename(project, run), 
	ctlfile = filename(rundir, run, ".ctl"), 
	...
)
}

\arguments{
  \item{run}{a run name, as for \code{\link{NONR}}}
  \item{project}{parent of the run directory}
  \item{rundir}{path to the run directory}
  \item{ctlfile}{name of the control stream}
  \item{\dots}{ignored}
}


\value{
character
}
\references{\url{http://metrumrg.googlecode.com}}

\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{as.spec}}
		\item \code{\link{NONR}}
	}
}
\keyword{manip}

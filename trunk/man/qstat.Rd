\name{qstat}
\alias{qstat}

\title{Invoke SGE qstat}
\description{
 Thin wrapper in R for SGE Command Line qstat.
}
\usage{
qstat(u=Sys.info()[["login"]],f='',...)
}

\arguments{
  \item{u}{character}
  \item{f}{character}
  \item{\dots}{extra arguments to `qstat' or \code{system}}
}
\details{
  This is a wrapper.
  All argument values are character.  Values are concatenated using \code{paste}, with 
  the usual effects if any arguments have length greater than one.  Arguments
  may be used more than once, and will be represented in the order received.
  Passing NA results in the argument instance being dropped.  Use an empty string
  as the value for a flag that does not take a value, e.g. \sQuote{f}.
}
\value{character}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{qsub}}
	}
}
\examples{
\dontrun{qstat()}
}
\keyword{manip}


\name{qsub}
\alias{qsub}

\title{Build Commands to Invoke SGE qsub}
\description{
 Builds one or more commands suitable for invoking Sun Grid Engine's \code{qsub} in a 
 shell.
}
\usage{
qsub(command, ...)
}

\arguments{
  \item{command}{character}
  \item{\dots}{extra arguments}
}
\details{
  This is almost a wrapper, except that it does not actually make the system call.
  All argument values are character.  Values are concatenated using \code{paste}, with 
  the usual effects if any arguments have length greater than one.  Arguments
  may be used more than once, and will be represented in the order received.
  Passing NA results in the argument instance being dropped.  Use an empty string
  as the value for a flag that does not take a value, e.g. \sQuote{soft}. Quote the \sQuote{@}
  argument in backticks.
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
qsub('nm.pl',`@`='options',hard='',i='stdin',soft='',hard='',N=c('Run3c','Run3e'),hold_jid=c(NA,'Run3c'))
#[1] 'qsub -@ options -hard  -i stdin -soft  -hard  -N Run3c  nm.pl'               
#[2] 'qsub -@ options -hard  -i stdin -soft  -hard  -N Run3e -hold_jid Run3c nm.pl'
}
\keyword{manip}


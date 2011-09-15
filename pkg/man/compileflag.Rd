\name{compileflag}
\alias{compileflag}

\title{Develop a Value for a Compiler Resource on the Grid}
\description{
License management under Sun Grid Engine may require an argument to \code{qsub}
of, e.g., \sQuote{-l compile=1} (in the case where the compiler is \sQuote{ifort}). This function
calculates that value flexibly.
}
\usage{
compileflag(compiler, mappings = list(ifort = 1), ...)
}

\arguments{
  \item{compiler}{name of the compiler in use}
  \item{mappings}{values for the flag, per compiler}
  \item{\dots}{ignored}
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runCommand}}
	}
}\keyword{manip}


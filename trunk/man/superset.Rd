\name{superset}
\alias{superset}
\title{Augment NONMEM Input with Table Output}
\description{Given the name of a NONMEM run, create a superset
of data that has all of the run input data and any data recoverable
from the output of table records.}
\usage{
superset(
	run, 
	project = getwd(), 
	rundir = filename(project, run), 
	ctlfile = filename(rundir, run, ".ctl"), 
	key = character(0), 
	read.input = list(
		read.csv, 
		header = TRUE, 
		as.is = TRUE, 
		na.strings = "."
	), 
	read.output = list(
		read.table, 
		header = TRUE, 
		as.is = TRUE, 
		skip = 1, 
		comment.char = "", 
		check.names = FALSE
	), 
	...
)
}
\arguments{
  \item{run}{a run name, as for \code{\link{NONR}}}
  \item{project}{parent of the run directory}
  \item{rundir}{path to the run directory}
  \item{ctlfile}{name of the control stream}
  \item{key}{the object model for the input data}
  \item{read.input}{a methodology for acquiring the input}
  \item{read.output}{a methodology for acquiring the output}
  \item{\dots}{ignored}
}
\details{
	Given a run name and project directory, \code{superset} figures out the 
	run directory and location of a NONMEM control stream. It reads the control
	stream to identify the run-time location of input and output files, 
	as well as the "ignore" (and/or "accept") criteria that relate extent
	of input records to extent of output records.  \code{read.input} and
	\code{read.output} are lists consisting of functions and arguments
	appropriate for reading input and output file formats, respectively.
	The ignore criteria will be reconstructed per row so that output can be
	mapped unambiguously to input.  Alternatively, if \code{key} is provided,
	it is used as an object model to allow an inferential left join of output
	onto intput; this approach is riskier, but can back fill NA cells with 
	values that are otherwise constant within left-subsets of the key.

}
\value{logical}
\references{\url{http://metrumrg.googlecode.com}}

\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{raised.keyed}}
	}
}
\keyword{manip}

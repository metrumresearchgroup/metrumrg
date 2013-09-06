\name{superset}
\alias{superset}
\alias{ignored}
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
		as.is = TRUE
	), 
	read.output = list(
		read.table, 
		header = TRUE, 
		as.is = TRUE, 
		skip = 1, 
		comment.char = "", 
		check.names = FALSE
	), 
	exclusive=NULL,
	digits=5,
	...
)
ignored(
	run, 
	project = getwd(), 
	rundir = filename(project, run), 
	ctlfile = filename(rundir, run, ".ctl"), 
	read.input=list(read.csv,header=TRUE,as.is=TRUE),
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
  \item{exclusive}{character vector of output column names to exclude; or logical: whether to keep columns with re-used names}
  \item{digits}{significant digits for assessing informativeness when \code{exclusive=NULL}}
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
	mapped unambiguously to input. A column with \code{run} as its name
	is bound to the input data, showing 1 where a record was visible to NONMEM,
	and 0 otherwise.
	
	Normally, \code{superset} tries to bind output columns directly to input.
	Alternatively, if \code{key} is provided,
	it is used as an object model to allow an inferential left join of output
	onto input; this approach is riskier, but can back fill NA cells with 
	values that are otherwise constant within left-subsets of the key.
	
	During integration, naming convention of
	the input is retained, and output column names are mapped by position, 
	using the control stream input criteria. Output tables are restored to 
	input dimensions using the "ignore" criteria, then checked for length: 
	currently, \code{superset} ignores output tables having fewer rows than 
	the input, as well as output tables whose row count is not a multiple 
	of input row count.
	
	Output tables may contain versions of input columns.  Disposition depends
	on the value of \code{exclusive}. If a character vector, it lists columns 
	to exclude from output. If \code{TRUE}, all columns with re-used names 
	will be dropped. If \code{FALSE}, such columns will be renamed 
	(*.n, where n is table number). If \code{NULL}, only informative 
	columns will be retained and renamed.  A column is informative if any 
	element is informative.  An element is informative if it is newly generated
	(not NA and not zero, but original is NA) or if it is an alteration (non-NA,
	and different from non-NA original). If the column pair can be interpreted
	as numeric, "different" is determined using only the first \code{digits} digits.
	
	Only the first instance of any column among successive output tables is retained.
	
	\code{ignored} takes a subset of \code{superset} arguments and returns just
	a logical vector showing which input rows are dropped. Note that it is 
	possible to invoke either function using just \code{run}, just \code{rundir},
	or just \code{ctlfile}, supplying \code{project} as necessary.  Normally these 
	three are tightly coordinated and may be inferred from each other.  

}
\note{
	In the control stream, avoid use of FIRSTONLY, as this alters the number
	of rows.  Tables created using FIRSTONLY can be summarized by \code{superset}
	if \code{key} is provided.  Note that when \code{key} is provided, innocuous
	warnings result (e.g. 'nothing to merge') if items are tabled that are 
	already present in the original data set.
	
	Early versions of these functions supported the documented ignore/accept
	syntax, e.g. \code{REP.GT.0}.  As of v. 5.52, Fortran 95 syntax is also
	supported, e.g. \code{REP > 0}.  
}
\value{
	\itemize{
	\item \code{superset}: data.frame where row count is a multiple of (usually equal to) input row count 
	\item \code{ignored} a logical vector indicating which rows were dropped.
	}
}
\references{\url{http://metrumrg.googlecode.com}}

\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
		\item \code{\link{dataSynthesis}}
		\item \code{\link{raised.keyed}}
	}
}
\keyword{manip}

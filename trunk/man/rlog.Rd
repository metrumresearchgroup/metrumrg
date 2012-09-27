\name{rlog}
\alias{rlog}
\alias{purgeRunDir}
\title{Generate a Combined Run Log from Multiple NONMEM Runs}
\description{
  Generates a combined run log across specified runs.  Appends/overwrites 
  existing file.  Attempts to purge certain files in run directory. 
}
\usage{
rlog(
	run, 
	project = getwd(), 
	append = TRUE,
	tool = 'nm7',
	file = filename(project,'CombRunLog.csv'),
	rundir = filename(project, run),
	nmlog = file.path(rundir,'NonmemRunLog.csv'),
	nmout = filename(rundir,run,'.lst'),
        purge=TRUE,
 	...
)
purgeRunDir(
	dirs, 
	debug = FALSE, 
	standard = c(
		"^F[ISRCMP]", "^OU", "^nonmem", "^nul$", 
		"WK", "LNK$", "fort", "^nm", "lnk$", 
		"set$", "^gar", "^temp", "^tr", "^new", 
		"^Run\\\\d+\\\\.o\\\\d+$", "^prsizes", "log$", 
		if (!debug) c("^FD", "^PR")
	), 
	extra = character(0), 
	pattern = c(standard, extra), 
	...
)
}
\arguments{
  \item{run}{vector of run names (numbers)}
  \item{project}{path to the directory containing run subdirectories}
  \item{append}{\code{TRUE}: append file; \code{FALSE}: overwrite file}
  \item{tool}{\sQuote{nm6} or \sQuote{nm7}: controls methodology. Defaults to 'nm7' as of version 5.18.}
  \item{file}{The run-generic output file to write. See details.}
  \item{rundir}{path to run subdirectory}
  \item{nmlog}{The run-specific input file to read (nm6 only). See details.}
  \item{nmout}{The run-specific NONMEM output file to read (nm7 only). See details.}
  \item{purge}{whether to remove files as defined in purgeRunDir}
  \item{dirs}{character vector: run directories to purge}
  \item{debug}{if TRUE, do not delete FDATA and PREDERR by default}
  \item{standard}{default files to purge}
  \item{extra}{other files to purge}
  \item{pattern}{files to purge: trumps \code{standard} and \code{extra} if specified}
  \item{\dots}{passed to other functions, such as \code{runstate} and \code{purgeRunDir}}
}

\details{
	\code{rundir}, \code{nmlog}, and \code{nmout} can be vector; or scalar, in which case they may 
	contain \sQuote{*}, to be replaced with run names on a per-run basis. 
  
	To suppress file deletion, supply \code{FALSE} for 
	\code{purge}.
  
	To suppress log creation on disk, supply a zero length argument for \code{file}.
	
	%If \code{test} is present, rlog assumes the run is in progress and does not attempt file deletion.
}
\value{
  Combined runlog is returned invisibly in the unilog format.  Side effect: if
  \code{file} is specified, runlog format is written to disk.
}
\note{
  As of version 5.18, if the attempt to create a unilog for the run
  fails, rlog generates a surrogate entry for parameter "min" with value "-1" (moment: "status").
  Formerly, the error was converted to a warning and reported at the console.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{written by Bill Knebel; modified by Tim Bergsma.}
\seealso{
	\itemize{
		\item \code{\link{as.unilog.run}}
		\item \code{\link{as.runlog.file}}
		\item \code{\link{runstate}}
	}
}
\keyword{manip}

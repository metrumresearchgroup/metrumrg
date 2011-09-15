\name{rlog}
\alias{rlog}
\title{Generate a Combined Run Log from Multiple NONMEM Runs}
\description{
  Generates a combined run log across specified runs.  Appends/overwrites 
  existing file.  Attempts to purge certain files in run directory. 
}
\usage{
rlog(
	run, 
	project = getwd(), 
	boot = FALSE, 
	append = TRUE,
	tool = 'nm6',
	file = filename(project,'CombRunLog.csv'),
	rundir = filename(project, run, if(boot) '.boot' else ''),
	nmlog = file.path(rundir,'NonmemRunLog.csv'),
	nmout = filename(rundir,run,'.lst'),
	pattern = if(boot)c('^F','^nonmem.exe','^P','^O','^Run') else '^FD',
	...
)
}
\arguments{
  \item{run}{vector of run names (numbers)}
  \item{project}{path to the directory containing run subdirectories}
  \item{boot}{Were these runs in \file{.boot} directories?}
  \item{append}{\code{TRUE}: append file; \code{FALSE}: overwrite file}
  \item{tool}{\sQuote{nm6} or \sQuote{nm7}: controls methodology}
  \item{file}{The run-generic output file to write. See details.}
  \item{rundir}{path to run subdirectory}
  \item{nmlog}{The run-specific input file to read (nm6 only). See details.}
  \item{nmout}{The run-specific NONMEM output file to read (nm7 only). See details.}
  \item{pattern}{search patterns for files to delete (regular expressions)}
  \item{\dots}{passed to other functions, such as \code{runstate}}
}

\details{
	\code{rundir}, \code{nmlog}, and \code{nmout} can be vector; or scalar, in which case they may 
	contain \sQuote{*}, to be replaced with run names on a per-run basis. 
  
	To suppress file deletion, supply a zero-length argument such as \code{NULL} for 
	\code{pattern}.
  
	To suppress log creation on disk, supply a zero length argument for \code{file}.
	
	If \code{test} is present, rlog assumes the run is in progress and does not attempt file deletion.
}
\value{
  Combined runlog is returned invisibly in the unilog format.  Side effect: if
  \code{file} is specified, runlog format is written to disk.
}
\references{\url{http://mifuns.googlecode.com}}
\author{written by Bill Knebel; modified by Tim Bergsma.}
\seealso{
	\itemize{
		\item \code{\link{as.unilog.run}}
		\item \code{\link{as.runlog.file}}
		\item \code{\link{runstate}}
	}
}
\keyword{manip}

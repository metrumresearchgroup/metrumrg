\name{runlog}
\alias{as.file.runlog}
\alias{as.runlog.file}
\alias{as.runlog.unilog}
\alias{as.unilog.lst}
\alias{as.unilog.pxml}
\alias{as.unilog.runlog}
\alias{unilog}
\alias{runlog}
\title{Convert Model Metadata to Various Formats}
\description{
  Metadata from successful model runs, e.g. NONMEM, comes in various formats,
  some of which are version dependent.  NONMEM6 and 7 outputs are interconverted
  with support for the conventional run log format as well as a universal format.
}
\usage{
runlog()
unilog()
as.runlog.file(file, ...)
as.unilog.lst(file, run, tool, ...)
as.unilog.pxml(x, run, tool = 'nm7', ...)
as.unilog.runlog(x, tool = 'nm6', ...)
as.runlog.unilog(x, ...)
as.file.runlog(x, file = 'NonmemRunLog.csv', header = FALSE, quote = FALSE, na = '.', ...)
}
\arguments{
  \item{x}{data.frame in runlog or unilog format}
  \item{file}{file name for metadata file, e.g. \file{NonmemRunLog.csv}, \file{3.lst}}
  \item{header}{whether to include column names in output}
  \item{quote}{whether to quote cell contents}
  \item{na}{string to represent \code{NA} in output}
  \item{\dots}{passed arguments}
  \item{run}{name (number) of the model run corresponding to the data}
  \item{tool}{the tool that created the metadata:  currently \sQuote{nm6} or \sQuote{nm7}}
}

\details{
  These functions are not typically needed by the user, since \code{rlog} 
  serves as an interface.
  
  Metadata may reside in \file{lst} files (NONMEM primary output), in \file{NonmemRunLog}
  files (created for NONMEM6 by \pkg{metrumrg} INFN routine) or in \file{ext} files (NONMEM7
  secondary output).  \code{as.unilog.lst}, \code{as.runlog.file}, and \code{as.pxml.ext} 
  (documented elsewhere) read these formats.  \emph{pxml} is an internal xml format
  produced by \code{as.pxml.ext} and converted to unilog by \code{as.unilog.pxml}.
  
  The \dfn{runlog} format by convention has the columns: \code{prob, moment, min, 
  cov, mvof, p1...pn,} and (possibly) \code{run}. \code{p1} through \code{pn} are (an arbitrary number
  of) parameters for that run.  The others give, respectively, the problem statement,
  a flag to identify relative standard error percent, minimization status, covariance
  status, minimum value of the objective function, and run name (number).  The primary
  values for each variable are given in a single record.  Where available, a 
  second record gives \sQuote{PRSE}, with non-informative entries as necessary.  Usually the
  header is not present in files.
  
  The \dfn{unilog} format is fully normalized.  It has the columns: \code{tool, run, parameter,
  moment, value}.  Moment is, for instance, \sQuote{estimate} or \sQuote{prse}.  Typically 
  all cells are filled and meaningful.  The value column is handled as text for
  maximum compatibility across data types.  The term \sQuote{parameter} is used informally:
  several items typically captured are not really parameters \emph{per se}.
  
  Unilog and runlog formats are interconvertible via \code{as.runlog.unilog} and 
  \code{as.unilog.runlog}.  \code{as.file.runlog} creates the traditional disk file from the 
  runlog format; writing unilog to disk is left to conventional strategies. 
  
  \code{runlog} and \code{unilog} return zero-row data.frames with the corresponding formats.
}
\value{
\code{as.file.runlog} is used for side effects.  All others return data.frame
in the runlog or unilog format.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\note{
These functions are not vectorized: their first arguments are expected to 
represent a single model run.

As of 5.18, as.unilog.run tries to give a more meaningful value of 'min' than just 1 or 0 for NONMEM 7.
Not supported on NONMEM 7.1.2, since it requires the .xml file available under 7.2.
}
\seealso{
	\itemize{
		\item \code{\link{as.unilog.run}}
		\item \code{\link{rlog}}
		\item \code{\link{as.pxml.ext}}
	}
}
\examples{
runlog()
unilog()
ext <- c(
	'TABLE NO.  1: First Order: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION',
	' ITERATION    THETA1       THETA2       THETA3       SIGMA(1,1)   OMEGA(1,1)   OBJ',
	'            0  1.70E+0  1.02E-1  2.90E+1  0.00E+0  1.17E+0  11.570086639848398',
	'            2  1.78E+0  1.06E-1  3.05E+1  0.00E+0  1.08E+0  9.377909428896904',
	'            4  1.91E+0  1.05E-1  3.14E+1  0.00E+0  8.96E-1  8.983605357031118',
	'            6  1.94E+0  1.01E-1  3.20E+1  0.00E+0  9.06E-1  8.940731060922468',
	'            8  1.93E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940110966224346',
	'           10  1.94E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940101673144566',
	'           11  1.94E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940101673144566',
	'  -1000000000  1.94E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940101673144566',
	'  -1000000001  6.28E-1  7.36E-3  1.25E+0  0.00E+0  5.44E-1  0.'
)
file <- textConnection(ext)
pxml <- as.pxml.ext(file)
close(file)
pxml
unip <- as.unilog.pxml(pxml,run=3)
unip
runlogfile <- c(
'1001  2 compartment base,,  0,  1,  0.1439E+5,  0.2E+1,  0.4E+1,  0.2E+1,  0.5E+1,  
0.1E+1,  0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  0.1E+0,  0.0E+0,  0.0E+0,  
0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  0.0E+0,  
0.0E+0,  0.0E+0,  0.0E+0,  0.3E+0,  0.1E+1,  0.0E+0,  0.0E+0, ,',
'1001  2 compartment base,RSE,,,,0,0,0,0,0,0,0,0,0,0,  0.0E+0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,  0.0E+0,  0.0E+0,0,0, ,'
)
runlogfile <- sub('\\n','',runlogfile)
file <- textConnection(runlogfile)
rlg  <- as.runlog.file(file)
close(file)
rlg
as.runlog.unilog(unip)
as.unilog.runlog(rlg)
}
\keyword{manip}


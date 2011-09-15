\name{fixProblem}
\alias{fixProblem}
\alias{fixFile}
\alias{explicitPath}
\alias{extractPath}
\alias{resolve}
\alias{scavenge}
\alias{tabfile}
\alias{parfile}
\alias{msffile}
\alias{extfile}
\title{Clean Up a Control Stream}
\description{
   These functions are used to re-write a control stream for 
   \code{NONR(checkrunno=TRUE,...)}.
}
\usage{
fixProblem(x,run)
fixFile(x,run)
explicitPath(x)
extractPath(x)
resolve(file,dir)
scavenge(expr,lines)
extfile(ctlfile,dir,extreg,...)
tabfile(ctlfile,dir,tabreg='(?<!par)\\\\.tab',...)
parfile(ctlfile,dir,parreg='par\\\\.tab',...)
msffile(ctlfile,dir,msfreg='^(?!\\\\$MSFI).*\\\\.msf',...)
}

\arguments{
  \item{x}{character vector, i.e. read from control stream file}
  \item{run}{run designator to substitute at predefined locations}
  \item{file}{an absolute or relative filepath}
  \item{dir}{a directory}
  \item{expr}{a regular expression to locate relevant lines}
  \item{lines}{lines in which to search for expr}
  \item{ctlfile}{character vector representing the control stream}
  \item{extreg}{a regular expression to locate an arbitrary file}
  \item{tabreg}{a regular expression to locate the \file{.TAB} file}
  \item{parreg}{a regular expression to locate the \file{par.TAB} file}
  \item{msfreg}{a regular expression to locate the \file{.MSF} file}
  \item{\dots}{ignored}
}
\details{
\code{fixProblem} tries to substitute the run name in the NONMEM \$PROBLEM statement
with the current run name.  It looks for any number of leading spaces, and then RUN or RUN\# 
(optional).  The next space delimited word is replaced.

\code{fixFile} tries to replace the run name in filenames ending in \file{.msf}, \file{par.tab},
or \file{.tab}.  Currently it relies on \code{dirname} and \code{basename}, which use 
the same file separator on 'Nix and Windows.  \code{basename} fails if there is no 
path separator, e.g., \code{FILE=40.tab}.  \code{fixFile} pre-processes its argument with
\code{explicitPath}, as a work-around.  Lines containing \sQuote{MSFI} formerly were coerced using explicit
paths, but are now ignored.

\code{explicitPath} finds lines that contain \file{.tab} or \file{.msf} but not \sQuote{/}.  It tries to 
place \sQuote{./} before \file{run.msf} or \file{run.tab}.

\file{extractPath} isolates the file portion of a line of text.  The file portion consists of a sequence
of nonspace characters following \sQuote{MSF=}, \sQuote{MSFO=}, or \sQuote{FILE=}.  Spaces may occur
before and after the equality sign.  This function is currently used by \code{runNonmem}
to locate the tabfile, parfile, and msffile.

\code{resolve} does nothing to absolute filepaths in \code{file}, but expresses others (those
beginning with \sQuote{.}) relative to \code{dir}.

\code{tabfile}, \code{parfile}, and \code{msffile} calculate corresponding filepaths from their
arguments, relying on \code{extfile} as the common engine.
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runNonmem}}
		\item \code{\link{dataSynthesis}}
	}
}
\examples{
prob <- c(
	'$  1000 desc',
	'$1000 desc',
	'$RUN1000 desc',
	'$ RUN#1000 desc',
	'$ RUN# 1000 desc'
)
fixProblem(prob,2000)
msf <- c(
	'$EST MAXEVAL=9999 MSF=../1000.MSF',#standard
	'$EST MAXEVAL=9999 msf=../1000.MSF',#case change
	'$EST MAXEVAL=9999 MSF =  ../1000.MSF',#one or two spaces
	'$EST MAXEVAL=9999 MSFO=../1000.MSF',#optional O
	'$EST MAXEVAL=9999 MSF=../1000.msf',#case change
	'$EST MAXEVAL=9999 MSF=../1000.msf INTER',#trailing text
	'$EST MAXEVAL=9999 MSF=../1000/1000.msf',#non-target path elements
	'$EST MAXEVAL=9999 MSF=1000.msf',#no path
	'$MSFI 1000.MSF',#non-target file name
	'$INPUT etc'#non-target
)
fixFile(msf,2000)
tab <- c(
	'ONEHEADER NOPRINT FILE=../1000.TAB',
	'ONEHEADER NOPRINT FILE  = ../1000.TAB',
	'ONEHEADER NOPRINT FILE=../1000par.TAB',
	'ONEHEADER NOPRINT file=../1000par.tab',
	'ONEHEADER NOPRINT file=../1000par.tab',
	'ONEHEADER NOPRINT file= 1000par.tab',
	'ONEHEADER NOPRINT FILE=../1000.TAB',
	'ONEHEADER NOPRINT FILE=~/1000.TAB'
)
fixFile(tab,2000)
files <- c(
	'1000.msf',
	'1000.tab',
	'./1000.tab',
	'../1000.tab',
	'$TABLE EVID FILE=1000.tab',
	'$TABLE EVID FILE=1000par.tab',
	'$MSFI 1000.msf'
)
explicitPath(files)
extractPath(msf)
extractPath(tab)
resolve('./file.ext',dir='project/pk')
resolve('../file.ext',dir='project/pk/')
scavenge('.tab',lines=c('text file.TAB','text file.csv'))
ctlfile <- c('text file=../100.tab etc','text file=../100par.tab etc','text file=../100.msf etc')
tabfile(ctlfile,dir='projectdir/100')
parfile(ctlfile,dir='projectdir/100')
msffile(ctlfile,dir='projectdir/100')
}		
\keyword{manip}


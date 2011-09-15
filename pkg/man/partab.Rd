\name{partab}
\alias{partab}
\alias{wikitab}
\title{Construct a Parameter Table}
\description{Merges data from \code{rlog} and \code{lookup} to describe model run parameters.}
\usage{
partab(
	run, 
	project = getwd(), 
	boot = FALSE, 
	tool = "nm6", 
	file = filename(rundir, run, ".ctl"), 
	rundir = filename(project, run, if (boot) ".boot" else ""), 
	nmlog = file.path(rundir, "NonmemRunLog.csv"), 
	nmout = filename(rundir, run, ".lst"), 
	x = params(within = within, by = by, type = type, ...), 
	within = ctl2xml(readLines(file)), 
	by = "name", 
	as = c(NA, "estimate", "unit", "prse"), 
	type = "parameter", 
	append = NULL, 
	pattern = NULL, 
	...
)
wikitab(
	run, 
	project = getwd(), 
	boot = FALSE, 
	tool = "nm7", 
	file = filename(rundir, run, ".ctl"), 
	rundir = filename(project, run, if (boot) ".boot" else ""), 
	nmlog = file.path(rundir, "NonmemRunLog.csv"), 
	nmout = filename(rundir, run, ".lst"), 
	x = params(within = within, by = by, type = type, ...), 
	within = ctl2xml(readLines(file)), 
	by = "model",
	type = "wiki", 
	append = NULL, 
	pattern = NULL, 
 	...
)
}
\arguments{
  \item{run}{scalar run name (number)}
  \item{project}{path to the directory containing run subdirectories}
  \item{boot}{Were these runs in \file{.boot} directories?}
  \item{tool}{\sQuote{nm6} or \sQuote{nm7}: controls methodology}
  \item{file}{the run-specific input file to read, contra \code{rlog}}
  \item{rundir}{path to run subdirectory}
  \item{nmlog}{The run-specific input file to read (nm6 only). See \code{rlog}.}
  \item{nmout}{The run-specific NONMEM output file to read (nm7 only). See \code{rlog}.}
  \item{x}{a vector of parameter names (character)}
  \item{within}{an XML \sQuote{document} containing parameter elements (character)}
  \item{by}{the parameter attribute by which to interpret \code{x}}
  \item{as}{vector of parameter attributes as which to describe \code{x}. Where NA, the text 
  value of the element (if any) is returned.}
  \item{type}{the element type to seek in \sQuote{within}}
  \item{append}{ignored; NULL is passed to \code{rlog}}
  \item{pattern}{ignored; NULL is passed to \code{rlog}}
  \item{\dots}{passed to \code{lookup} and \code{rlog}}
}
\details{The idea here is to associate pre-specified model-specific identifiers with model output.
Identifiers derive from some text file, such as a NONMEM model control stream.  \code{partab}
calls \code{lookup} to recover the identifiers, and \code{rlog} to access the results.  The 
common key is \sQuote{name} by default in \code{lookup}, and \sQuote{parameter} by default in \code{rlog}.
Caution is advisable when using these terms in other ways.

\code{lookup} only allows one value for \code{as}.  Here, \code{as} can be a vector.  The special
value \code{NA} returns the text element for the parameter (cs. \code{NULL} in \code{lookup}).

This mechanism allows messy model details to be abstracted to a more appropriate location than 
a modeling script.  Output is suitable as an argument to \code{latex} in package \code{Hmisc}.

\code{wikitab} is like \code{partab}, but only returns the 'model' attribute (default) and the 
element text for elements of type 'wiki' (default).  It tries to guess the parameter names from
the model attribute, using wikimath conventions (see \code{\link{tos}}), and then fold in the 
related details from the run log.  \code{wikitab} is more restrictive than \code{partab},
but it lets all the usual parameter details arise from a single specification thereof.
}
\value{data.frame}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{lookup}}
		\item \code{\link{params}}
  	\item \code{\link{rlog}}
    \item \code{\link{wikimath}}
    \item \code{\link{tos}}
	}
}
\keyword{manip}


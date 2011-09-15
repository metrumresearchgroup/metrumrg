\name{runNonmem}
\alias{runNonmem}

\title{Process a Request for NONMEM Invocation}
\description{
 The heart of NONR, this function handles file-level details and dispatches 
 the other major functions, especially \code{runCommand} and \code{PLOTR}.
}
\usage{
runNonmem(
	run,
	command,
	project,
	boot,
	urgent,
	checkrunno,
	diag,
	fdata,
	epilog,
	dvname,
	logtrans,
	grp,
	grpnames,
	cont.cov,
	cat.cov,
	par.list,
	eta.list,
	missing,
	invisible,
	checksum,
	grid,
	nice,
	udef,
	compile,
	execute,
	split,
	plotfile=plotfilename(run,project,grp),
	runext = if(boot) '.boot' else if(grid) '.lock' else '',
	rundir = filename(project,run,runext),
	outfile = filename(rundir,run,'.lst'),
	streams = project,
	ctlfile = filename(streams,run,'.ctl'),
	remove = c(
		"^F[ISRCMP]","^OU","^nonmem", "^nul$",
		"WK","LNK$","fort","^nm","lnk$","set$",
		"^gar","INT","^temp","^tr","^new",
		if(fdata)c('^FD','^PR')
	),
	sync=if(boot)'n'else'y',
	interface='nm.pl',
	...,
	perm.cond=NULL
)
}
\arguments{
  \item{run}{see NONR}
  \item{command}{see NONR}
  \item{project}{see NONR}
  \item{boot}{see NONR}
  \item{urgent}{see NONR}
  \item{checkrunno}{see NONR}
  \item{diag}{see NONR}
  \item{fdata}{see NONR}
  \item{epilog}{see NONR}
  \item{dvname}{see NONR}
  \item{logtrans}{see NONR}
  \item{grp}{see NONR}
  \item{grpnames}{see NONR}
  \item{cont.cov}{see NONR}
  \item{cat.cov}{see NONR}
  \item{par.list}{see NONR}
  \item{eta.list}{see NONR}
  \item{missing}{see NONR}
  \item{invisible}{see NONR}
  \item{checksum}{see NONR}
  \item{grid}{see NONR}
  \item{nice}{see NONR}
  \item{udef}{see NONR}
  \item{compile}{see NONR}
  \item{execute}{see NONR}
  \item{split}{see NONR}
  \item{plotfile}{see PLOTR}
  \item{runext}{an extension for the run directory}
  \item{rundir}{the directory in which run will occur}
  \item{outfile}{see \code{dataSynthesis}}
  \item{streams}{where to find control streams}
  \item{ctlfile}{the original control stream}
  \item{remove}{regular expressions for files to purge}
  \item{sync}{whether the R process should wait for the run to complete}
  \item{interface}{the name of a function to prepare NONMEM command lines}
  \item{\dots}{extra arguments for other functions}
  \item{perm.cond}{passed to PLOTR; defined here to prevent partial matching of \code{pe} argument to \code{qsub}}
}
\value{used for side effects}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{NONR}}
		\item \code{\link{dataSynthesis}}
		\item \code{\link{runCommand}}
	}
}
\keyword{manip}


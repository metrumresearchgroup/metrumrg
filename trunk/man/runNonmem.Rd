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
	wait,
	#urgent,
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
	rundir = filename(project,run),
	outfile = filename(rundir,run,'.lst'),
	streams = project,
	ctlfile = filename(streams,run,'.ctl'),
	purge=TRUE,
	sync=if(wait)'y'else'n',
	interface='nm.pl',
	...,
	perm.cond=NULL
)
}
\arguments{
  \item{run}{see NONR}
  \item{command}{see NONR}
  \item{project}{see NONR}
  \item{wait}{see NONR}
  %\item{urgent}{see NONR}
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
  %\item{runext}{an extension for the run directory}
  \item{rundir}{the directory in which run will occur}
  \item{outfile}{see \code{dataSynthesis}}
  \item{streams}{where to find control streams}
  \item{ctlfile}{the original control stream}
  \item{purge}{whether to remove files as defined in purgeRunDir}
  \item{sync}{whether the R process should wait for the run to complete}
  \item{interface}{the name of a function to prepare NONMEM command lines}
  \item{\dots}{extra arguments for other functions}
  \item{perm.cond}{passed to PLOTR; defined here to prevent partial matching of \code{pe} argument to \code{qsub}}
}
\value{the result of the call to \code{\link{runCommand}}; or empty string if \code{command} is empty string.}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{NONR}}
		\item \code{\link{dataSynthesis}}
		\item \code{\link{runCommand}}
		\item \code{\link{purgeRunDir}}
	}
}
\keyword{manip}


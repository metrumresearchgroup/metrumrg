\name{runCommand}
\alias{runCommand}

\title{Issue the System Call That Invokes NONMEM}
\description{
	If \code{udef} is \code{FALSE}, \code{command} is passed to \code{nm.pl} with supporting arguments.  If \code{grid}
	is \code{TRUE}, the result is passed to \code{qsub} with supporting arguments.  If \code{boot}
	is \code{TRUE}, \sQuote{&} is appended to the result.  Each element of the result is executed
  in succession using a system call customized by platform.
}
\usage{
runCommand(
	command,
	...,
	run,
	rdir,
	boot,
	urgent,
	checksum,
	grid,
	udef = FALSE,
	ctlfile,
	outfile,
	perl = if (nix()) 'perl -S' else if (!invisible) 'cmd /K perl -S' else 'cmd /C perl -S',
	intern = invisible,
	minimized = invisible,
	invisible = FALSE,
	split = grid,
	N = glue('Run', run, if (split) c('c', 'e') else NULL),
	o = rdir,
	e = rdir,
	L=if(split & interface=='nm.pl')c(compileflag(compiler(config(dirname(command)))),NA)else NA,
	hold_jid = if (split) c(NA, glue('Run', run, 'c')) else NA,
	V = '',
	j = 'y',
	q=if(split)
	    c(
	        'compile.q',
	        if(urgent)'all.q' else 'bootstrap.q'
	    )
	else 
	    if(!execute)
	        'compile.q'
            else if(urgent)'all.q' else 'bootstrap.q',
	sync = if (boot) 'n' else 'y',
	shell = 'n',
	b = 'y',
	cwd = '',
	compile = TRUE,
	execute = TRUE,
	background=FALSE,
	interface = 'nm.pl'
)
}
\arguments{
	\item{command}{a command to pass to \code{system}}
  \item{run}{run name}
  \item{rdir}{run directory}
  \item{boot}{see \code{NONR}}
  \item{urgent}{see \code{NONR}}
  \item{checksum}{see \code{NONR}}
  \item{grid}{see \code{NONR}}
  \item{udef}{see \code{NONR}}
  \item{ctlfile}{see \code{runNonmem}}
  \item{outfile}{see \code{runNonmem}}
  \item{perl}{a character string to invoke perl}
  \item{intern}{see \code{NONR}, passed to \code{system}}
  \item{minimized}{see \code{NONR}, passed to \code{system}}
  \item{invisible}{see \code{runNonmem}}
  \item{split}{whether compile and execute should be run separately}
  \item{N}{passed to \code{qsub}}
  \item{o}{passed to \code{qsub}}
  \item{e}{passed to \code{qsub}}
  \item{L}{passed to \code{qsub} as an instance of \sQuote{l}}
  \item{hold\_jid}{passed to \code{qsub}}
  \item{V}{passed to \code{qsub}}
  \item{j}{passed to \code{qsub}}
  \item{q}{passed to \code{qsub}}
  \item{sync}{passed to \code{qsub}.  Bootstrap runs usually occur in large quantities,
  whereas SGE has an internal limitation on number of synchronized processes.  Therefore,
  no attempt is currently made to synchronize bootstrap runs.}
  \item{shell}{passed to \code{qsub}}
  \item{b}{passed to \code{qsub}}
  \item{cwd}{passed to \code{qsub}}
  \item{compile}{passed to \code{nm.pl}}
  \item{execute}{passed to \code{nm.pl}, influences default for \code{q}}
  \item{background}{\code{TRUE} appends \sQuote{l} to command lines to put the 
  process in the background.  Defunct?}
  \item{interface}{the name of a function to prepare NONMEM command lines}
  \item{\dots}{passed to \code{nm.pl} and \code{qsub}}
}
\details{
	The argument \sQuote{L} represents a possibly-constituative instance of \code{qsub}'s \sQuote{l},
but is not called \sQuote{l} so that other instances of \sQuote{l} (multiple are allowed)
will not accidentally override it.  Users can override intentionally, of course.

\code{N}, \code{L}, and \code{hold_jid} are coordinated so that if a run is split, compile status is flagged
on the compile run, and the execute run waits for compile to finish.

\sQuote{q} is handled specially.  When overriding, be sure to pass a character vector of
length one for a normal run, and of length two if \code{split} is \code{TRUE} (the default when
\code{grid} is \code{TRUE}).  By default, all standalone compile-only runs are diverted to 
\sQuote{compile.q}, as well as all compile halves of split runs.  By default, all execute-only
runs as well as the execute halves of split runs are diverted to \sQuote{all.q} if urgent,
and \sQuote{bootstrap.q} otherwise.
}
\value{Used for side effects}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runNonmem}}
	}
}
\keyword{manip}


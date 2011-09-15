\name{nm.pl}
\alias{nm.pl}
\alias{autolog.pl}
\title{Build Commands to Invoke NONMEM}
\description{
 Format a set of commandlines, suitable for submission to a command shell, to
 operate the executable for an NMQual-mediated NONMEM installation.  \code{nm.pl}
 is the default for NONMEM 7.1.2 and earlier.  \code{autolog.pl} is the default 
 for NONMEM 7.2.0 and later.
}
\usage{
nm.pl(
	command,
	ctlfile,
	outfile=NULL,
	perl='perl',
	checksum=TRUE,
	compile=TRUE, 
	execute=TRUE,
	split=FALSE,
	...
)
autolog.pl(
	command, 
	rdir, 
	run, 
	perl = "perl", 
	compile = TRUE, 
	execute = TRUE, 
	split = FALSE,
	config = file.path(dirname(command),"log.xml"),
	mode = "run", 
	...
)
}

\arguments{
  \item{command}{path for the installation-specific variant of \file{nm.pl}}
  \item{ctlfile}{path for a control stream}
  \item{outfile}{path for output}
  \item{perl}{how to invoke Perl on this system}
  \item{checksum}{whether to conduct checksums}
  \item{compile}{whether to compile NONMEM}
  \item{execute}{whether to execute the compiled NONMEM}
  \item{split}{whether to compile and execute as separate steps}
  \item{rdir}{run directory}
  \item{run}{run name}
  \item{config}{path to a configuration file (sensu NMQual 8)}
  \item{mode}{process mode}
  \item{\dots}{ignored}
}
\details{
	If \code{split} is \code{TRUE}, two commands are returned, having the mode flags \sQuote{c} and \sQuote{e}, respectively.
	\code{nm.pl} drops the \sQuote{c} or \sQuote{e} argument if \code{split} is FALSE and
	both \code{compile} and \code{execute} are TRUE.  In the identical case,
	\code{autolog.pl} passes \sQuote{ce}. 
	
	\code{autolog.pl} returns a run directory and run name, rather than input
	and output file paths as in \code{nm.pl}. \code{autolog.pl} does not 
	support \code{checksum}. By default, \code{autolog.pl} expects a config
	file called \sQuote{log.xml} in the same directory as \code{command}; it also
	assumes the mode is \sQuote{run}.  See also \url{http://nmqual.googlecode.com}.
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runCommand}}
	}
}
\examples{
nm.pl('/nm6/test/nm6.pl','1.ctl')
nm.pl('/nm6/test/nm6.pl','1.ctl','1.out',checksum=FALSE)
nm.pl('/nm6/test/nm6.pl','1.ctl',execute=FALSE)
nm.pl('/nm6/test/nm6.pl','1.ctl',compile=FALSE)
nm.pl('/nm6/test/nm6.pl','1.ctl',split=TRUE)
autolog.pl('/nm72/nmqual/autolog.pl','/home/ubuntu/test/3',3)
}
\keyword{manip}


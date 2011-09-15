\name{electronicAppendix}
\alias{electronicAppendix}
\title{Make an FDA-style Electronic Appendix from a Subversioned Directory.}
\description{Make an FDA-style Electronic Appendix from a Subversioned Directory.}
\usage{electronicAppendix(x, as, pattern = NULL, recursive = TRUE, ignore.case = TRUE, zip = FALSE, ...)
}
\arguments{
  \item{x}{a Subversioned directory, e.g. a checkout of a repository or subdirectory thereof.}
  \item{as}{a file path for a directory to be created}
  \item{pattern}{passed to \code{dir}}
  \item{recursive}{passed to \code{dir}}
  \item{ignore.case}{passed to \code{dir}}
  \item{zip}{logical indicating whether to compress \code{as} to a zipped file.}
  \item{\dots}{ignored}
}
\details{
	FDA submissions may require electronic copies of file trees, where 
	text files must have the extension \sQuote{txt}. This function
	creates a copy of \code{x} using \code{svn export} (a system call).
	It fixes the names of those files that Subversion considers text,
	and optionally compresses the resulting directory to a zipped file 
	(also a system call).  Not tested on all platforms, but should
	work fine on most Unix-alikes.
	
	For finer control over what is considered text, see \code{svnMarkAsText}
	and \code{svnMarkAsNonText}.
	
	Note that non-subversioned files will not be included in the electronic appendix. 
	An error results if any file is subversioned but missing.
	
	\code{as} must be specified as a directory name.  If \code{zip} is \code{TRUE}, .zip will be added.
	
	\code{as} cannot pre-exist unless \code{zip} is true, in which
	case \code{as} will be renamed temporarily while its namesake is zipped and unlinked.
}
\value{used for side effects}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{dir}}
		\item \code{\link{svnIsText}}
		\item \code{\link{svnMarkAsText}}
		\item \code{\link{svnMarkAsNonText}}
	}
}
\keyword{manip}

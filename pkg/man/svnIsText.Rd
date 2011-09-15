\name{svnIsText}
\alias{svnIsText}
\title{Check Whether Subversioned Files are Text.}
\description{Check wether Subversion considers a file to be text.}
\usage{svnIsText(x, ...)
}
\arguments{
  \item{x}{character vector of file names.}
  \item{\dots}{ignored}
}
\details{See \url{http://subversion.apache.org/faq.html#binary-files}.
	When a file is added, Subversion guesses whether it is text
	or binary, storing the decision implicitly as the value (or
	lack thereof) of the svn:mime-type property.  This function
	makes that decision explicit. Returns NA for files that are not Subversioned.  
	If you don't agree with Subversion's determination, you can alter it 
	manually, or with \code{svnMarkAsText} or \code{svnMarkAsNonText}.
}
\value{logical}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{svnMimeType}}
		\item \code{\link{svnMarkAsText}}
		\item \code{\link{svnMarkAsNonText}}
	}
}
\keyword{manip}

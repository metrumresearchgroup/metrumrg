\name{metrumrgURL}
\alias{metrumrgURL}
\title{View a metrumrg Package File}
\description{Open a file in the metrumrg package using a default viewer.}
\usage{
metrumrgURL(url, lib.loc = NULL, browser = getOption("browser"), encodeIfNeeded = FALSE)
}
\arguments{
  \item{url}{a file path relative to the package root}
  \item{lib.loc}{passed to \code{system.file}}
  \item{browser}{passed to \code{browseURL}}
  \item{encodeIfNeeded}{passed to \code{browseURL}}
}
\details{
	Example files included in the \pkg{metrumrg} package
	can be difficult to find.  This function combines
	system.file and browseURL to open the package-relative
	url in a viewer.  
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{browseURL}}
		\item \code{\link{system.file}}
	}
}
\examples{
metrumrgURL('example/wikimath/wikimath.pdf')
}
\keyword{manip}


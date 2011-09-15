\name{bracket}
\alias{bracket}
\title{Create an XML Tag}
\description{
  Create an open or close XML tag of the form \code{<x>} or \code{</x>}.
}
\usage{
bracket(x, close = FALSE, ...)
}
\arguments{
  \item{x}{an element name}
  \item{close}{whether the tag is a close tag; \code{FALSE} by default}
  \item{\dots}{name-value pairs of attributes for an open tag}
}
\details{
  \code{close} can be a vector.  An attempt will be made to supress attributes
  for \sQuote{close} tags.
}
\value{
 character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{attribute}}
		\item \code{\link{as.xml.character}}
	}
}
\examples{
bracket(rep('name',2),close=FALSE:TRUE,row=1,col=2)
}
\keyword{manip}


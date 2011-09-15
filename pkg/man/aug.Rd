\name{aug}
\alias{aug}
\title{Augment a List-like Object}
\description{Add named elements to an object.}
 \usage{
aug(x, ...)
}
\arguments{
  \item{x}{dispatch argument}
  \item{\dots}{name-value pairs}
}
\details{
  For each extra argument, an attempt is made to assign its value to
  a like-named element of \code{x} (typically a list or data.frame). The 
  augmented object is returned.  Similar to \code{transform}, but simpler.
}
\value{
like \code{x}
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{transform}}
	}
}
\examples{
head(aug(Theoph,study='b'))
}
\keyword{manip}

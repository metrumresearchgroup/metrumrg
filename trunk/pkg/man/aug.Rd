\name{aug}
\alias{aug}
\title{Augment a List-like Object}
\description{Add named elements to an object.}
 \usage{
aug(`_data`, ...)
}
\arguments{
  \item{_data}{a list-like object}
  \item{\dots}{name-value pairs}
}
\details{
  For each extra argument, an attempt is made to assign its value to
  a like-named element of \code{x} (typically a list or data.frame). The 
  augmented object is returned.  Similar to \code{transform}, but simpler.
  Each argument will be evaluated first in an environment constructed from 
  \code{_data}.
}
\value{
like \code{_data}
}
\references{\url{http://metrumrg.googlecode.com}}
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

\name{random}
\alias{random}
\alias{random.data.frame}
\title{Select Random Members of an Object}
\description{
	Select random members of an object.
}
\usage{
	\method{random}{data.frame}(x,n=6L,...)
}
\arguments{
  \item{x}{An object, e.g. data.frame}
}
\details{
	The functions \code{head} and \code{tail} show the first or last parts
	of an object.  \code{random} shows an arbitrary portion.  Generic, with
	a method defined for \code{data.frame}.
}
\value{same class as x}
\references{\url{http://metrumrg.googlecode.com}}
\author{Jim Rogers}
\seealso{
	\itemize{
		\item \code{\link{head}}
	}
}
\examples{
random(Theoph)
}
\keyword{manip}


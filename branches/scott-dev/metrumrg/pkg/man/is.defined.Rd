\name{is.defined}
\alias{is.defined}
\title{Test Whether Values are Defined}
\description{Test for data definition by negating the result of \code{is.na}.}
 \usage{
is.defined(x)
}
\arguments{
  \item{x}{object}
}
\details{
	The function \code{is.na} is applied to the object, and the opposite of the
	result is returned.  This yields a more intuitive syntax for certain tests.
}
\value{
logical 
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{is.na}}
	}
}
\examples{
is.defined(letters)
is.defined(Theoph)
}
\keyword{manip}

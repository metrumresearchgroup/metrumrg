\name{ord.matrix}
\alias{ord.matrix}
\alias{ord.halfmatrix}
\alias{ord}
\title{Give the Order of a Symmetric Matrix or Half Matrix}
\description{
The dimensions of a symmetric matrix are equal; this value is, by definition,
the order of the matrix.  For class \code{halfmatrix}, the order is found using
a quadratic expression with length as an argument.
}
\usage{
\method{ord}{matrix}(x, ...)
\method{ord}{halfmatrix}(x, ...)
}
\arguments{
  \item{x}{
symmetric matrix or halfmatrix
}
  \item{\dots}{
extra arguments, ignored
}
}
\details{
It is an error if \code{x} is a matrix but not symmetric.  If \code{x} is a 
vector of appropriate length its order can be found by specifying the method
explicitly, even if \code{x} is not classed as halfmatrix:  \code{ord.halfmatrix(x)}.
}
\value{
scalar numeric
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\code{\link{is.square.matrix}}, 
	\code{\link{isSymmetric.matrix}}
	\code{\link{half.matrix}}
}
\examples{
ord(diag(4))
ord(half(diag(4)))
ord.halfmatrix(1:10)
}
\keyword{manip}


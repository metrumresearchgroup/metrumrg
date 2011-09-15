\name{posmat}
\alias{posmat}
\title{Coerce a Matrix to be Positive Definite}
\description{
For a square matrix with an all-positive diagonal, elements are limited to 
6 significant digits by rounding the diagonal and shrinking off diagonal elements
toward zero.  The off-diagonals are reduced by 3 percent as necessary
until the determinant is positive.
}
\usage{
posmat(x,...)
}
\arguments{
  \item{x}{
matrix with only positive diagonal elements
}
\item{\dots}{extra arguments, ignored}
}
\value{
matrix
}
\references{\url{http://mifuns.googlecode.com}}
\author{Leonid Gibianski, modified by Tim Bergsma}

\examples{
posmat(matrix(c(10.00006,20.00006,-30,40),2,2))
posmat(matrix(rep(100,4),2,2))
}

\keyword{manip}


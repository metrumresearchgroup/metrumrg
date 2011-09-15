\name{is.square.matrix}
\alias{is.square.matrix}
\alias{is.square}
\title{Test Whether a Matrix is Square}
\description{
A matrix is square if its dimensions are equal.
}
\usage{
\method{is.square}{matrix}(x, ...)
}
\arguments{
  \item{x}{
matrix
}
  \item{\dots}{
extra arguments, ignored
}
}
\value{
logical
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\code{\link{ord.matrix}}, 
	\code{\link{isSymmetric.matrix}}
}
\examples{
is.square(diag(3))
}
\keyword{manip}


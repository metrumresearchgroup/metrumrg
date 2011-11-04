\name{is.diagonal}
\alias{is.diagonal}
\alias{is.offdiagonal}
\alias{is.fixed}
\alias{is.random}
\alias{is.iiv}
\alias{is.residual}
\title{Analyze Parameter Labels}
\description{
Analyze parameter labels of the form "THETA1" or "OMEGA2.3" or "SIGMA1.1".
\code{is.diagonal} and \code{is.offdiagonal} decode the numeric x.x portion
as indicating a region of a matrix.  \code{is.fixed} detects "THETA",
\code{is.iiv} detects "OMEGA", and \code{is.residual} detects "SIGMA",
reflecting conventional usage for population pk-pd.
\code{is.random} detects all "OMEGA" and "SIGMA".  All functions
are case-insensitive.
}
\usage{
is.diagonal(x)
is.offdiagonal(x)
is.fixed(x)
is.random(x)
is.iiv(x)
is.residual(x)
}
\arguments{
  \item{x}{character}
}
\details{Integer subscripts, as in "THETA1", are neither diagonal nor offdiagonal.}
\value{logical}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{offdiag}}
	}
}
\examples{
x <- c("THETA1","OMEGA1.2","SIGMA1.1"
is.diagonal(x)
is.offdiagonal(x)
is.fixed(x)
is.random(x)
is.iiv(x)
is.residual(x)
}
\keyword{manip}


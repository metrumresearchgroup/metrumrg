\name{is.alpha}
\alias{is.alpha}
\title{Test For Alphabetic Content}
\description{Verify that each element contains only letters.}
\usage{
is.alpha(x, ...)
}
\arguments{
  \item{x}{character}
  \item{\dots}{ignored}
}
\details{Verifies that each element in \code{x} contains only 
upper or lower case letters.}
\value{logical}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{is.latex.token}}
	}
}
\examples{
is.alpha(c('aB','a2'))
}
\keyword{manip}


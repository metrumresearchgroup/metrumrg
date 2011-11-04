\name{parameter2wiki}
\alias{parameter2wiki}
\title{Convert Parameter Labels to Wikimath}
\description{
Convert parameter labels for thetas, omegas, sigmas, and etas
to wikimath.  See \code{\link{wikimath}}.
}
\usage{
subscript(x, ...)
}
\arguments{
  \item{x}{character, or coercible to character}
  \item{\dots}{ignored}
}
\details{
This function expects a vector of labels such as "THETA1"
or "OMEGA2.1".  It converts them to their wikimath equivalents.
Thetas are coerced to the lower case representation,
whereas omegas, sigmas, and Etas will be upper case. The 
result can be passed conveniently to \code{wiki2latex} for 
table development.
}
\value{
character
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{wiki2latex}}
		\item \code{\link{wiki2parameter}}
		\item \code{\link{tos}}
		\item \code{\link{titleCase}}
	}
}
\examples{
x <- parameter2wiki(c("THETA1","OMEGA1.2","SIGMA1.1")
x
wiki2latex(x)
}
\keyword{manip}


\name{row2tabular}
\alias{row2tabular}
\title{Format Vector for Latex Tabular Row}
\description{Format a vector for use as a row in latex tabular environment.}
\usage{
row2tabular(x, ...)
}
\arguments{
	\item{x}{vector, coerced to character with \code{paste}}
  \item{\dots}{ignored}
}
\details{Elements are collapsed into a string, with ampersand as the separator.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{tabular.data.frame}}
	}
}
\examples{
row2tabular(names(Theoph))
}
\keyword{manip}


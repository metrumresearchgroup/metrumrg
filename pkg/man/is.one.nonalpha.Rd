\name{is.one.nonalpha}
\alias{is.one.nonalpha}
\title{Check That Each Element is a Single Non-letter}
\description{Verify that each element is just one non-letter.}
\usage{
is.one.nonalpha(x, ...)
}
\arguments{
  \item{x}{character}
  \item{\dots}{ignored}
}
\details{Used to help identify proper latex command words.}
\value{logical}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{is.latex.token}}
	}
}
\examples{
is.one.nonalpha(c(2,'b',23))
}
\keyword{manip}


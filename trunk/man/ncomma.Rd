\name{ncomma}
\alias{ncomma}
\title{Report Names as Comma-delimited}
\description{A convenience wrapper to report names as comma-delimited.}
 \usage{
ncomma(x,...)
}
\arguments{
  \item{x}{object}
  \item{\dots}{passed to \code{names()}}
}
\value{
character
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{names}}
	}
}
\examples{
ncomma(Theoph)
}
\keyword{manip}

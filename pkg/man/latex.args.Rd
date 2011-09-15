\name{latex.args}
\alias{latex.args}
\title{Format Latex Command Arguments}
\description{Format latex command arguments.}
\usage{
latex.args(x, ...)
}
\arguments{
  \item{x}{list or vector}
  \item{\dots}{ignored}
}
\details{Each element wrapped in curly braces; elements are strung together.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{wrap}}
			\item \code{\link{command}}
	}
}
\examples{
latex.args(c('arg1','arg2'))
}
\keyword{manip}


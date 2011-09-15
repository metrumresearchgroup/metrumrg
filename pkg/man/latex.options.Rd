\name{latex.options}
\alias{latex.options}
\title{Format Latex Command Options}
\description{Format latex command options.}
\usage{
latex.options(x, ...)
}
\arguments{
  \item{x}{list or vector}
  \item{\dots}{ignored}
}
\details{Elements are strung in a comma separated list,
optionally with \code{name=} syntax.  List is enclosed in 
square brackets.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{wrap}}
			\item \code{\link{command}}
			\item \code{\link{tagvalue}}
	}
}
\examples{
latex.options(c(ht=2,width=3))
}
\keyword{manip}


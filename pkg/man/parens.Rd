\name{parens}
\alias{parens}
\title{Wrap Text in Parentheses}
\description{Wrap text in parentheses.  Useful when formating latex tables.}
 \usage{
parens(x, ...)
}
\arguments{
  \item{x}{character}
  \item{\dots}{ignored}
}
\details{
	Text is wrapped in parentheses, with no intermediate spaces.
}
\value{
character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{glue}}
	}
}
\examples{
parens('x')
}
\keyword{manip}

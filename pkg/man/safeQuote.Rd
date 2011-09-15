\name{safeQuote}
\alias{safeQuote}
\title{
Single-quote Conditionally ~~
}
\description{
Place single quotes around text that contains space and is not already quoted. ~~
}
\usage{
safeQuote(x)
}
\arguments{
  \item{x}{
character~~
}
}
\details{
Text is already quoted if it begins with a single or double quote.}
\value{
character
}
\references{\url{http://mifuns.googlecode.com}}
\author{
Tim Bergsma
}
\seealso{
	\itemize{
		\item \code{\link{contains}}
		\item \code{\link{isSubversionedFile}}
	}
}
\examples{
safeQuote(c("text","more text",'"already quoted"'))
}
\keyword{manip}

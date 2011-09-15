\name{tagvalue}
\alias{tagvalue}
\title{Convert List to Tag and Value Format}
\description{Convert a list to tag and value format.}
\usage{
tagvalue(x, sep = "=", collapse = ",", ...)
}
\arguments{
  \item{x}{list}
  \item{sep}{the symbol to associate tags and values}
  \item{collapse}{the character to concatenated tag-value pairs}
  \item{\dots}{ignored}
}
\details{List names, where present, prefix list values, with the indicated
separator.}
\value{Character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{latex.options}}
	}
}
\examples{
tagvalue(list(a=1,b=2,3))
}
\keyword{manip}


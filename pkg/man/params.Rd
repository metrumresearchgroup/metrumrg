\name{params}
\alias{params}
\title{List Documented Parameters}
\description{
Returns a vector of parameters available in a specially-formatted 
XML document, preserving the order. 
}
\usage{
params(within, by = "name", type = "parameter", ...)
}
\arguments{
  \item{within}{an XML \sQuote{document} containing parameter elements (character)}
  \item{by}{the parameter attribute by which to interpret \code{x}}
  \item{type}{the element type to seek in \sQuote{within}}
  \item{\dots}{ignored}
}
\details{This function is argumented nearly identically to \code{lookup}, except
that \code{x} is missing.  Whereas in \code{lookup}, one is looking for details
about elements of \code{x}, in \code{params}, one is asking what those elements
may be.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{lookup}}
		\item \code{\link{partab}}
	}
}
\keyword{manip}


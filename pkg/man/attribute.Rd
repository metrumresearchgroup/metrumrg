\name{attribute}
\alias{attribute}
\title{Encode an Attribute for an XML Open Tag}
\description{
  Encode an attribute for an XML open tag, in the form \code{tag='x'}.
}
\usage{
attribute(x, tag, ...)
}
\arguments{
  \item{x}{character vector: attribute value(s)}
  \item{tag}{character (vector): attribute name}
  \item{\dots}{ignored}
}
\value{
character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{bracket}}
	}
}
\examples{
attribute(letters,'letter')
}
\keyword{manip}


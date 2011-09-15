\name{nest}
\alias{nest}
\title{Nest an XML Fragment in a Parent Element}
\description{
  Create matching open and close tags and embed an XML fragement between them.
}
\usage{
nest(x, tag, ...)
}
\arguments{
  \item{x}{an XML fragment}
  \item{tag}{an element name}
  \item{\dots}{ignored}
}
\details{
  Attributes are not supported.
}
\value{
  character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{bracket}}
		\item \code{\link{as.xml.data.frame}}
		\item \code{\link{as.pxml.ext}}
	}
}
\examples{
nest('some xml',tag='content')
}
\keyword{manip}


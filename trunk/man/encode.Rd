\name{encode}
\alias{encode}
\alias{encode.default}
\alias{encode.list}
\alias{encode.character}
\title{Encode Factor-like Levels and Labels as a Simple String}
\description{
	For compact storage, \code{encode} combines a set of levels and 
	labels (codes and decodes) into a simple string.  The default method
	converts its argument to character.  The list method operates element-wise,
	expecting and equal number of label elements, each of which have the same
	length as the corresponding element of x.
}
\usage{
	\method{encode}{default}(x, labels = NULL, sep = '/',...)
	\method{encode}{character}(x, labels = NULL, sep = '/',...)
	\method{encode}{list}(x, labels = NULL, sep = '/',...)
}
\arguments{
  \item{x}{typically character}
  \item{labels}{typically character}
  \item{sep}{character}
  \item{\dots}{ignored}

}
\details{
An 'encoding' must be at least 5 characters long, beginning and ending with two
instances of \code{sep}. Specified levels are likewise separated by double separators.
If a label (decode) is available for a level, it follows the corresponding level: the
two are separated by a single instance of \code{sep}.  Separators may be mixed
within an encoded vector, but not within an element.  \code{--0-male--1-female--} indicates
that the value 0 represents the concept "male" and the value 1 represents "female".
}
\value{
character
}
\references{\url{http://metrumrg.googlecode.com}}
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{as.spec}}
		\item \code{\link{specfile}}
		\item \code{\link{specification}}
		\item \code{\link{codes}}
		\item \code{\link{\%matches\%}}
	}
}
\examples{
a <- encode(
  x = list(
    c('M','F'),
    c(1:4)
  ),
  labels = list(
    c('male','female'),
    c('caucasian','asian','african',NA)
  )
)
b <- encode(c(1:2),c('pediatric','adult'))
a
b
}
\keyword{manip}

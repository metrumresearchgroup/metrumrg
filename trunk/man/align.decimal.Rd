\name{align.decimal}
\alias{align.decimal}
\alias{padded}
\title{Format Numeric Vectors Specially}
\description{Format a numeric vector so that decimal mark
is a constant distance from the end or beginning of each element.}
\usage{
align.decimal(x, decimal.mark = ".", ...)
padded(x,width = 4,...)
}
\arguments{
  \item{x}{numeric}
  \item{decimal.mark}{character indicating decimal}
  \item{width}{numeric indicating total digits}
  \item{\dots}{ignored}
}
\details{for \code{align.decimal}, each element is formatted separately using
\code{prettyNum}, then the character results are 
padded with spaces on the right, so that the decimals
align.  Whole numbers without the decimal mark get an 
extra space in its place.

For \code{padded}, x (numeric) is formatted with zero decimal places, no decimal, and enough
leading zeros to ensure width \code{width}.

}
\value{character}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{tabular.data.frame}}
	}
}
\examples{
align.decimal(c(.12, 1.2, 12.0))
padded(c(1,10,100,1000),5)
}
\keyword{manip}


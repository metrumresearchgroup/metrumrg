\name{align.decimal}
\alias{align.decimal}
\title{Format a Vector Aligning on Decimal}
\description{Formats a numeric vector so that decimal mark
is a constant distance from the end of each element.}
\usage{
align.decimal(x, decimal.mark = ".", ...)
}
\arguments{
  \item{x}{numeric}
  \item{decimal.mark}{character indicating decimal}
  \item{\dots}{ignored}
}
\details{Each element is formatted separately using
\code{prettyNum}, then the character results are 
padded with spaces on the right, so that the decimals
align.  Whole numbers without the decimal mark get an 
extra space in its place.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{tabular.data.frame}}
	}
}
\examples{
align.decimal(c(.12, 1.2, 12.0))
}
\keyword{manip}


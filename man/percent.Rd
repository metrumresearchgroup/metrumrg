\name{percent}
\alias{percent}
\title{
Calculate Percent.
}
\description{
Format a decimal as percent, specifying significant digits.
}
\usage{
percent(x, digits = 3, ...)
}
\arguments{
  \item{x}{numeric}
  \item{digits}{passed to \code{signif}}
  \item{\dots}{ignored}
}
\value{numeric}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{signif}}
	}
}
\examples{
percent(.53421)	
}
\keyword{manip}


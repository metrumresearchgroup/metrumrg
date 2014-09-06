\name{toSAS.timepoint}
\alias{toSAS.timepoint}
\title{Convert Timepoint to SAS Format}
\description{Convert timepoint objects to SAS format for writing XPT files}
\usage{
\method{toSAS}{timepoint}(x,...)
}
\arguments{
  \item{x}{timepoint class}
  \item{\dots}{ignored}
}
\details{\pkg{SASxport} defines \code{toSAS} and calls it on each column
when writing XPT files. The \code{timepoint} method indicates number of seconds
since the start of 1960-01-01.}
\value{numeric}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{timepoint}}
	}
}
\examples{
toSAS(as.mDate(0))
}
\keyword{manip}


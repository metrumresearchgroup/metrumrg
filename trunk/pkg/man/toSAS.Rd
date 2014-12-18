\name{toSAS.mDateTime}
\alias{toSAS.mDateTime}
\alias{toSAS.mDate}
\alias{toSAS.mTime}
\title{Convert Timepoint to SAS Format}
\description{Convert timepoint objects to SAS format for writing XPT files}
\usage{
\method{toSAS}{mDateTime}(x, format="", format.info=NULL)
\method{toSAS}{mDate}(x, format="", format.info=NULL)
\method{toSAS}{mTime}(x, format="", format.info=NULL)
}
\arguments{
  \item{x}{subclass of timepoint}
  \item{format}{SAS format name}
  \item{format.info}{Table of SAS format information}
}
\details{\pkg{SASxport} defines \code{toSAS} and calls it on each column
when writing XPT files. The \code{mDateTime} method returns the integer number of seconds
since the start of 1960-01-01. The \code{mDate} method returns the integer number of days
since 1960-01-01.The \code{mTime} method returns the number of seconds
since midnight.
}
\value{numeric}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{timepoint}}
	}
}
\examples{
if(require(SASxport)) toSAS(as.mDateTime('1960-01-01 00:00')) # 0
if(require(SASxport)) toSAS(as.mDate('1960-01-02')) # 1
if(require(SASxport)) toSAS(as.mTime('00:01')) # 60

}
\keyword{manip}


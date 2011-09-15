\name{ftable2data.frame}
\alias{ftable2data.frame}
\title{Convert ftable to data.frame}
\description{Convert ftable to data.frame As Displayed}
\usage{
ftable2data.frame(x,...)
}
\arguments{
  \item{x}{ftable}
  \item{\dots}{ignored}
}
\details{
	\code{as.data.frame.ftable} does indeed convert an \code{ftable} to \code{data.frame},
	but it gives a stacked result.  Use this function to 
	give something more like what is displayed at the prompt (suitable for
	passing to latex).
}
\value{data.frame}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\examples{
ftable(mtcars[c("cyl", "vs", "am", "gear")])
as.data.frame(ftable(mtcars[c("cyl", "vs", "am", "gear")]))
ftable2data.frame(ftable(mtcars[c("cyl", "vs", "am", "gear")]))
}
\keyword{manip}


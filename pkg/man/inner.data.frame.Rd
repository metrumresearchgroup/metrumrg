\name{inner.data.frame}
\alias{inner}
\alias{inner.data.frame}

\title{
	Limit Data to Inner Quantiles by Imputing NA.
}
\description{
\code{inner} is generic.  \code{inner.data.frame} imputes \code{NA} for
cells within columns that represent extreme quantiles. By default, the
\sQuote{inner} 95 percent of each column is preserved.

}
\usage{
\method{inner}{data.frame}(
	x,
	prob=0.95,
	tail=0.5*(1-prob),
	lo=tail,
	hi=prob+tail,
	include.lowest=TRUE,
	include.highest=TRUE,
	preserve=character(0),
	id.var=character(0),
	measure.var=setdiff(names(x),c(preserve,id.var)),
	na.rm=FALSE,
	...
)
}
\arguments{
  \item{x}{data.frame}
  \item{prob}{the fraction of data to preserve}
  \item{tail}{the fraction of data to ignore at each extreme}
  \item{lo}{the probability below which data will be ignored}
  \item{hi}{the probability above which data will be ignored}
  \item{include.lowest}{whether to preserve values at probability equal to \code{lo}}
  \item{include.highest}{whether to preserve values at probability equal to \code{hi}}
  \item{preserve}{vector of names for columns to preserve but ignore}
  \item{id.var}{vector of names for columns that indicate data subsets}
  \item{measure.var}{vector of names for columns to limit}
  \item{na.rm}{passed to \code{quantile}}
  \item{\dots}{passed to \code{fun.aggregate}}
}
\details{
	\code{prob} and \code{tail} are not actually used internally, but serve
	only to calculate symmetric defaults for \code{lo} and \code{hi}.  If
	the latter are supplied, the former are ignored.  Tails need not be 
	symmetric.
	
	By default, all columns are classified as \code{measure.var}: an attempt
	will be made to limit such. Columns classified as \code{preserve} will
	simply be passed through to the result.  Columns classified as \code{id.var}
	specify row subsets that are limited independently of eachother.  See examples.
}
\value{a data frame with the same rows, columns, row order, and column order as x 
(except for dropped columns)}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}


\seealso{
	\itemize{
		\item \code{\link{helpPrepare}}
	}
}
\examples{
x <- airquality[c(1:10,32:41),]
x
inner(x,id.var=c('Month','Day'),na.rm=TRUE)#identity
inner(x,id.var='Month',preserve='Day',na.rm=TRUE)#quantiles within Month
inner(x,preserve=c('Month','Day'),na.rm=TRUE)#quantiles across all rows
inner(x,measure.var=c('Ozone','Solar.R','Wind','Temp'),na.rm=TRUE)#like previous, but dropping Month, Day
inner(rock,prob=0.5)
}
\keyword{manip}


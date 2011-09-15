\name{panel.hist}
\alias{panel.hist}
\alias{panel.bar}
\alias{unitHist}
\title{Plot Histograms Flexibly}
\description{
	\code{histogram} and \code{panel.histogram} in package \pkg{lattice} (expected) are univariate.  Here,
  we present a bivariate histogram panel function for use, e.g., with \code{stripplot}.
  \code{panel.bar} adds line segments, appropriate for reference lines in a histogram.
  \code{unitHist} calls \code{hist} but returns histogram specifications that have been
  rescaled so that maximum bar height is 1.
}
\usage{
panel.hist(x,y,level,horizontal,col.line,fill,factor,border=col.line,col=fill,offset=-0.5,font,fontface,...)
panel.bar(x,y,level,horizontal,col,col.line,fill,factor,font,fontface,...)
unitHist(x, plot = FALSE, ...)
}
\arguments{
  \item{x}{vector}
  \item{y}{vector}
  \item{level}{the level at which to draw the histogram, typically supplied by \code{panel.stratify}}
  \item{horizontal}{typically \code{TRUE}}
  \item{col}{fill color (ignored for \code{panel.bar})}
  \item{col.line}{border color if \code{border} not supplied}
  \item{fill}{fill color if \code{col} not supplied}
  \item{factor}{factor by which to expand or attenuate bar heights relative to unity}
  \item{border}{border color}
  \item{offset}{amount to raise the base of each bar relative to the level}
  \item{\dots}{passed to other functions}
  \item{font}{ignored}
  \item{fontface}{ignored}
  \item{plot}{passed to \code{hist}}
}
\value{
\code{unitHist} returns class histogram with an extra element named \sQuote{heights}.  
\sQuote{heights} is proportional to density, but normalized relative to maximum density.
 Panel functions are used for side effects.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{panel.stratify}}
	}
}
\keyword{manip}


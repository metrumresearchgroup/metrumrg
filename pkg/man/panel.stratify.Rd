\name{panel.stratify}
\alias{panel.stratify}
\title{Handle Each Level of a Stripplot Separately}
\description{
Just as \code{panel.superpose} handles each group of data separately, \code{panel.stratify}
handles each \sQuote{level} of data separately.  Typically, levels are the unique
values of \code{y} (\code{horizontal==TRUE}) that result from a call to \code{stripplot} or \code{bwplot}.
  The default panel functions treat all levels simultaneously.  Plotting some 
  transformation of the data (e.g. density polygons for each level) is much easier if 
  the levels are presented individually.
}
\usage{
panel.stratify(
	x, 
	y, 
	type = 'p', 
	groups = NULL, 
	pch = if (is.null(groups)) 
	plot.symbol$pch else superpose.symbol$pch, 
	col, 
	col.line = if (is.null(groups)) 
	plot.line$col else superpose.line$col, 
	col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col, 
	font = if (is.null(groups)) 
	plot.symbol$font else superpose.symbol$font, 
	fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily, 
	fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface, 
	lty = if (is.null(groups)) 
	plot.line$lty else superpose.line$lty, 
	cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex, 
	fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill, 
	lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd, 
	horizontal = FALSE, 
	panel.levels = 'panel.xyplot', 
	..., 
	jitter.x = FALSE, 
	jitter.y = FALSE, 
	factor = 0.5, 
	amount = NULL
)
}
\arguments{
  \item{x}{See \code{panel.xyplot}}
  \item{y}{See \code{panel.xyplot}}
  \item{type}{See \code{panel.xyplot}}
  \item{groups}{See \code{panel.xyplot}}
  \item{pch}{See \code{panel.xyplot}}
  \item{col}{See \code{panel.xyplot}}
  \item{col.line}{See \code{panel.xyplot}}
  \item{col.symbol}{See \code{panel.xyplot}}
  \item{font}{See \code{panel.xyplot}}
  \item{fontfamily}{See \code{panel.xyplot}}
  \item{fontface}{See \code{panel.xyplot}}
  \item{lty}{See \code{panel.xyplot}}
  \item{cex}{See \code{panel.xyplot}}
  \item{fill}{See \code{panel.xyplot}}
  \item{lwd}{See \code{panel.xyplot}}
  \item{horizontal}{See \code{panel.xyplot}}
  \item{panel.levels}{a function to handle each unique level of the data}
  \item{\dots}{See \code{panel.xyplot}}
  \item{jitter.x}{See \code{panel.xyplot}}
  \item{jitter.y}{See \code{panel.xyplot}}
  \item{factor}{See \code{panel.xyplot}}
  \item{amount}{See \code{panel.xyplot}}
}
\details{
\code{panel.stratify} is defined almost identically to \code{panel.xyplot}.  \code{panel.levels}
is analogous to \code{panel.groups}.  \code{panel.levels} may want to handle special cases
of \code{col}, which may be missing if \code{groups} is \code{NULL} and may be \code{NA} if \code{groups} is 
not \code{NULL} (set to \code{NA} by \code{panel.superpose}).
  
\code{x} and \code{y} are split into subsets by whichever of them represents levels (\code{y} if 
  \code{horizontal} is \code{TRUE}, \code{x} otherwise).  Corresponding subsets of \code{x} and \code{y} are 
  forwarded one at a time, along with the other arguments, to \code{panel.levels}.  
  Additionally, the current value of \code{level} as well as the complete vector of 
  \code{levels} are available to \code{panel.levels}.
}
\value{used for side effects}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{panel.covplot}}
		\item \code{\link{panel.densitystrip}}
		\item \code{\link{panel.hist}}
		\item \code{\link{panel.xyplot}}
	}
}
\keyword{manip}


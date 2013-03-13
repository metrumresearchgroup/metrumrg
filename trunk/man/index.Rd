\name{index}
%\alias{[.digest}
\alias{[.conditioned}
%\alias{as.best.digest}
\alias{as.conditioned}
\alias{as.conditioned.digest}
\alias{as.conditioned.isolated}
\alias{as.conditioned.keyed}
%\alias{as.digest}
%\alias{as.digest.data.frame}
%\alias{as.digest.digest}
%\alias{as.digest.keyed}
%\alias{as.digest.nm}
\alias{as.isolated}
\alias{as.isolated.digest}
\alias{as.isolated.isolated}
\alias{as.isolated.keyed}
\alias{as.keyed.nm}
\alias{as.roles}
\alias{as.roles.digest}
\alias{as.roles.keyed}
\alias{as.motif}
\alias{as.motif.digest}
\alias{as.motif.keyed}
\alias{as.motif.motif}
\alias{as.motif.nm}
%\alias{digest}
\alias{format.legacy}
%\alias{head.digest}
\alias{index}
\alias{index.conditioned}
\alias{index.digest}
\alias{index.isolated}
\alias{index.keyed}
\alias{index.nm}
\alias{legacy}
\alias{legacy<-}
\alias{motif}
\alias{plot.conditioned}
\alias{plot.digest}
\alias{plot.isolated}
\alias{plot.keyed}
\alias{roles}
\alias{splom.conditioned}
\alias{splom.digest}
\alias{splom.keyed}

\title{Visualize Static Data Relationships}
\description{
Cascading diagnostic plotting for data frames and data digests.
}

\usage{
\method{as.conditioned}{digest}(x, ...)
\method{as.conditioned}{isolated}(x, ...)
\method{as.conditioned}{keyed}(x, roles = as.roles(x), ...)
\method{as.isolated}{digest}(x, ...)
\method{as.isolated}{isolated}(x, ...)
\method{as.isolated}{keyed}(x, ...)
\method{as.motif}{digest}(x, ...)
\method{as.motif}{keyed}(x, ...)
\method{as.motif}{motif}(x, ...)
\method{as.motif}{nm}(x, ...)
\method{as.roles}{digest}(x, ...)
\method{as.roles}{keyed}(x, motif = as.motif(x), ...)
\method{index}{conditioned}(x, roles = as.roles(x), ...)
\method{index}{digest}(x, motif = as.motif(x), roles = as.roles(x, motif = motif), ...)
\method{index}{isolated}(x, roles = as.roles(x), ...)
\method{index}{keyed}(x, roles = as.roles(x), ...)
\method{index}{nm}(x, density = 20, ...)
\method{plot}{conditioned}(x, roles = as.roles(x), ...)
\method{plot}{digest}(x, motif = as.motif(x), roles = as.roles(x, motif = motif), ...)
\method{plot}{isolated}(x, ...)
\method{plot}{keyed}(x, roles = as.roles(x), ...)
\method{splom}{conditioned}(x, data=NULL, roles = as.roles(x), main = "", xlab = "", pscales = 0, ...)
\method{splom}{digest}(x, data=NULL, motif = as.motif(x), roles = as.roles(x, motif = motif), main = "", xlab = "", pscales = 0, ...)
\method{splom}{keyed}(x, data=NULL, roles = as.roles(x), ...)
legacy(x, ...)
legacy(x) <- value
\method{format}{legacy}(x,...)
}
\arguments{
  \item{x}{object of dispatch}
  \item{roles}{named character vector with elements x, y, and z indicating columns to use for axes or for grouping}
  \item{motif}{a list with elements x, y, and z (optional), each of which are character vectors of column names in x, prioritizing alternative usage for plotting}
  \item{density}{integer: how many levels of x to include per panel}
  \item{main}{not passed to \code{splom}}
  \item{data}{not passed to \code{splom}}
  \item{xlab}{not passed to \code{splom}}
  \item{pscales}{not passed to \code{splom}}
  \item{value}{assigned to the legacy attribute of x}
  \item{\dots}{extra arguments passed to other methods}
}
\details{
This is an experimental system for generating diagnostic plots.  It was designed to 
work with the result of \code{digest}, but can also work with data frames and 
keyed data frames, etc.

\code{as.conditioned} resolves
a digest into a longer list of data frames (class \code{conditioned}), each of which
contain only single levels of key columns not mentioned in \code{roles}.  
\code{as.isolated} operates on conditioned or unconditioned keyed data frames, 
returning a list (class \code{isolated}) of data frames each having exactly one
non-key column.  Default methods are defined for making index plots and scatterplot
matrices of objects of class digest, conditioned, isolated, and keyed.  The \code{plot}
methods currently plot both. \code{head.digest} shows the head of each sub-result.

For purposes of plotting and conditioning, it is necessary to specify the roles
of the columns.  A \code{roles} object is a named vector with elements x, y, and z.
Typically, y is a dependent variable, x is an independent variable, and z is a 
grouping variable (optional).  y may be specified as '.', in which case it represents
the non-key variable in context.  

When plotting an entire digest all at once, it would be tedious to specify, in
advance, roles for every resulting view of the data. A \code{motif} is a general
strategy for assigning roles.  Unlike \code{roles}, it is a list.  Like \code{roles},
it has elements x, y, and (optional) z.  Each of these is a character vector 
of column names, typically consisting only of key columns and '.'.  Order is 
important:  supporting functions assign x, y, and z in the order given, removing
any assigned columns from consideration in later assignments.  For example, time
may be appropriate as x where available, and a subject identifier could be the 
alternative:  \code{motif=list(y='.',x=c('time','id'))}.  Methods exist for creating
default motifs for a variety of objects.  These are converted to roles in context.

The following are generic:
\code{as.motif},
\code{as.conditioned},
\code{index},
\code{as.roles},
\code{as.isolated}. 

\code{motif} and
\code{roles}

are aliases for
their \code{as.} counterparts.
}
\value{
	\code{as.conditioned} returns a list of objects each with class \code{conditioned, keyed, data.frame}.
	\code{as.isolated} returns a list of objects each with class \code{isolated, conditioned, keyed, data.frame}.
	\code{as.motif} and \code{motif} return an object of class \code{motif} with optional elements x, y, and z.
	\code{as.roles} and \code{roles} return a named character vector, mapping column usage to roles x, y, and z.
	\code{plot}, \code{splom}, and \code{index} return lists of trellis objects, possibly with deep nesting. 
	\code{plot.digest} gives a list with each element having members \code{splom} and \code{index}.
	\code{legacy} returns named character; \code{format.legacy} returns character.  The assignment version of 
	\code{legacy} requires named character and causes it to be appended to the legacy attribute of the object.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.keyed}}
		\item \code{\link{static}}
		\item \code{\link{digest}}
	}
}
\examples{
digest(Theoph,c('Subject','Time'))
head(digest(Theoph,c('Subject','Time')))
\dontrun{index(as.keyed(Theoph,'Subject'))}

}
\keyword{manip}


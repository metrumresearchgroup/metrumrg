\name{digest}
\alias{[.digest}
\alias{[.conditioned}
\alias{as.best.digest}
\alias{as.conditioned}
\alias{as.conditioned.digest}
\alias{as.conditioned.isolated}
\alias{as.conditioned.keyed}
\alias{as.digest}
\alias{as.digest.data.frame}
\alias{as.digest.digest}
\alias{as.digest.keyed}
\alias{as.digest.nm}
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
\alias{digest}
\alias{format.legacy}
\alias{head.digest}
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

\title{Diagnose and Visualize Static Data Relationships}
\description{
Break a data frame into components static on variants of a proposed key, and plot
them flexibly.
}

\usage{
\method{as.best}{digest}(x, ...)
\method{as.conditioned}{digest}(x, ...)
\method{as.conditioned}{isolated}(x, ...)
\method{as.conditioned}{keyed}(x, roles = as.roles(x), ...)
\method{as.digest}{data.frame}(x, key = character(0), strict = TRUE, mask = rep(FALSE, length(key)), ...)
\method{as.digest}{digest}(x, ...)
\method{as.digest}{keyed}(x, key = match.fun("key")(x), strict = TRUE, ...)
\method{as.digest}{nm}(x,key=match.fun('key')(x),...)
\method{as.isolated}{digest}(x, ...)
\method{as.isolated}{isolated}(x, ...)
\method{as.isolated}{keyed}(x, ...)
\method{as.keyed}{nm}(x, key = match.fun("key")(x), ...)
\method{as.motif}{digest}(x, ...)
\method{as.motif}{keyed}(x, ...)
\method{as.motif}{motif}(x, ...)
\method{as.motif}{nm}(x, ...)
\method{as.roles}{digest}(x, ...)
\method{as.roles}{keyed}(x, motif = as.motif(x), ...)
\method{head}{digest}(x, ...)
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
%[.conditioned(x, ...)
%[.digest(x, ...)
legacy(x, ...)
legacy(x) <- value
\method{format}{legacy}(x,...)
}
\arguments{
  \item{x}{object of dispatch}
  \item{key}{a vector of column names in \code{x} representing a proposed object hierarchy}
  \item{strict}{passed to \code{lyse}}
  \item{mask}{logical that controls the visibility of elments in \code{key}}
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
Well-constructed data tables typically admit a set of columns (a key), the interaction of which uniquely
distinguish all rows.  The columns may be ordered from most general to most specific, in which
case they may be thought of as an object hierarchy.  The hierarchy accounts for structural 
redunancy of identifier variables across rows.  When exploring data, it may be useful to 
remove such redundancy to focus on singular relationships within the data (e.g., like \code{static}).

\code{digest} recursively cleaves a data frame using appropriate subsets of a key. 
The original data frame and any dynamic residuals are cleaved using increasingly longer 
left subsets (1; 1,2; 1,2,3; etc.) of the proposed key. Effectively, this is a search 
for columns that are static on (i.e. are attributes of) various objects and sub-objects.
The static results of cleaving, if any, are further explored (if possible) with increasingly
shorter right subsets (e.g. 1,2,3; 2,3; 3) to detect any columns that are super-keyed:
i.e. are still strictly attributes of some sub-object, without appeal to more general
hierarchical levels.  \code{digest} returns a list of keyed data frames, such that each original
non-key column appears in exactly one data frame, together with the smallest necessary
set of key columns, and all siblings (like-keyed non-key columns). If indeed the proposed key
completely distinguishes all rows, the result consists only of static data frames.
Otherwise, the last data frame is dynamic. For columns that are constant in the data,
irrespective of the proposed key, the key of the sub-result has length zero. 
The resulting key is NA for a dynamic sub-result.

Additional analysis may be performed on sub-results. \code{as.conditioned} resolves
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
\code{as.digest},
\code{as.conditioned},
\code{index},
\code{as.roles},
\code{as.isolated}.  
\code{motif},
\code{roles}, and
\code{digest}
are aliases for
their \code{as.} counterparts.
}
\value{
	\code{as.digest} and \code{as.best.digest} return an object of class \code{digest}:  
	a list of keyed data frames, where the key may be of length zero, or may be NA.
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
	}
}
\examples{
digest(Theoph,c('Subject','Time'))
head(digest(Theoph,c('Subject','Time')))

}
\keyword{manip}


\name{digest}
\alias{[.digest}
%\alias{[.conditioned}
\alias{as.best.digest}
%\alias{as.conditioned}
%\alias{as.conditioned.digest}
%\alias{as.conditioned.isolated}
%\alias{as.conditioned.keyed}
\alias{as.digest}
\alias{as.digest.data.frame}
\alias{as.digest.digest}
\alias{as.digest.keyed}
\alias{as.digest.nm}
%\alias{as.isolated}
%\alias{as.isolated.digest}
%\alias{as.isolated.isolated}
%\alias{as.isolated.keyed}
\alias{as.keyed.nm}
%\alias{as.roles}
%\alias{as.roles.digest}
%\alias{as.roles.keyed}
%\alias{as.motif}
%\alias{as.motif.digest}
%\alias{as.motif.keyed}
%\alias{as.motif.motif}
%\alias{as.motif.nm}
\alias{digest}
%\alias{format.legacy}
\alias{head.digest}
%\alias{index}
%\alias{index.conditioned}
%\alias{index.digest}
%\alias{index.isolated}
%\alias{index.keyed}
%\alias{index.nm}
%\alias{legacy}
%\alias{legacy<-}
%\alias{motif}
%\alias{plot.conditioned}
%\alias{plot.digest}
%\alias{plot.isolated}
%\alias{plot.keyed}
%\alias{roles}
%\alias{splom.conditioned}
%\alias{splom.digest}
%\alias{splom.keyed}

\title{Diagnose Static Data Relationships}
\description{
Break a data frame into components static on variants of a proposed key.
}

\usage{
\method{as.best}{digest}(x, ...)
\method{as.digest}{data.frame}(x, key = character(0), strict = TRUE, ...)
\method{as.digest}{digest}(x, ...)
\method{as.digest}{keyed}(x, key = match.fun("key")(x), strict = TRUE, ...)
\method{as.digest}{nm}(x,key=match.fun('key')(x),...)
\method{as.keyed}{nm}(x, key = match.fun("key")(x), ...)
\method{head}{digest}(x, ...)
}
\arguments{
  \item{x}{object of dispatch}
  \item{key}{a vector of column names in \code{x} representing a proposed object hierarchy}
  \item{strict}{passed to \code{lyse}}
  \item{\dots}{passed to or from other functions}
}
\details{
Well-constructed data tables typically admit a set of columns (a key), the interaction of which uniquely
distinguish all rows.  The columns may be ordered from most general to most specific, in which
case they may be thought of as an object hierarchy.  The hierarchy accounts for structural 
redunancy of identifier variables across rows.  When exploring data, it may be useful to 
remove such redundancy to focus on singular relationships within the data (e.g., like \code{static}).

\code{digest} recursively cleaves a data frame using appropriate subsets of a key. 
The original data frame and any dynamic residuals are cleaved using increasingly longer 
left subsets (empty; 1; 1,2; 1,2,3; etc.) of the proposed key. Effectively, this is a search 
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
The resulting key for a dynamic sub-result is the last key tried (possibly different from
the proposed key, as elements may be removed from consideration if they are themselves
static on some prior key). Elements are named with their keys, pasted together with dots; 
except if the key is character(0), the name will be a single dot, or two dots for 
the last element if it is dynamic on the proposed key.
}
\note{
	\code{digest} is an alias for the generic \code{as.digest}.

}
\value{
	\code{as.digest} and \code{as.best.digest} return an object of class \code{digest}:  
	a list of keyed data frames, with names suggesting their keys ('.' for character(0), '..' for a dynamic data frame).
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.keyed}}
		\item \code{\link{static}}
		\item \code{\link{index}}
	}
}
\examples{
digest(Theoph,c('Subject','Time'))
head(digest(Theoph,c('Subject','Time')))
}
\keyword{manip}


\name{deranged}
\alias{as.moot}
\alias{as.rigged}
\alias{deranged.data.frame}
\alias{deranged.keyed}
\alias{deranged}
\alias{minus.moot}
\alias{moot.nm}
\alias{moot}
\alias{plus.rigged}
\alias{rig.nm}
\alias{rig}

\title{Modify Assembly of nm Objects}
\description{
These are special preparations of nm objects or their sources.  Proxy methods
for \code{Ops.keyed} are techniques for dynamic preparation.
}
\usage{
\method{moot}{nm}(x, scope = x$EVID == 2, disregard = c('TIME', 'DATETIME', 'DATE', 'TAFD', 'TAD','HOUR'), ...)
\method{rig}{nm}(x, n = 50, dateTime = FALSE, noDups = FALSE, ...)
\method{deranged}{keyed}(x, ...)
\method{deranged}{data.frame}(x, start, stop, result = start, dropStop = TRUE, ...)

as.moot(...)
as.rigged(...)

\method{minus}{moot}(x, y, ...)
\method{plus}{rigged}(x, y)


}
\arguments{
  \item{x}{dispatch argument}
  \item{y}{right operand to proxy method}
  \item{start}{name of column with \code{start} values}
  \item{stop}{name of column with \code{stop} values}
  \item{result}{name of column in which to place the result}
  \item{dropStop}{whether to drop the \code{stop} column before returning}
  \item{\dots}{extra arguments, usually ignored; see details}
  \item{scope}{an expression to evaluate within \code{x}, giving moot candidate rows}
  \item{disregard}{columns to disregard when testing for between-row differences}
  \item{n}{number of extra records to generate per subject}
  \item{dateTime}{whether to attempt derivation of timecourse from \code{DATETIME} instead of \code{HOUR}}
  \item{noDups}{whether to prohibit introduction of rows with times that already exist}

  
}
\details{
\code{deranged.data.frame}.  When a dataset attribute (column) is constant over 
some range of another discreet attribute, information may be stored more 
compactly by specifying the constant attribute once, together with the start 
and stop of the range. This function takes such a dataset and instantiates 
all implied records. Warning:  the function calls 
\code{seq(start,stop)} for each row, and makes no assumptions about whether the data 
is ascending, descending, stationary (\code{start==stop}), or undefined 
(one or more of \code{start} and \code{stop} is \code{NA}). \code{deranged.keyed} is just a wrapper that
preserves attributes.

\code{moot.nm} will identify rows that make no difference to NONMEM. If an \code{EVID:2}
row differs from the row just before it only with respect to temporal 
designations, the NONMEM result should not be affected. NONMEM should use 
the changed values from the first row in which non-temporal items change to 
estimate the midpoint of the prior interval. 

\code{rig.nm} generates extra rows, spaced across each subject's timecourse, 
for obtaining model predictions in NONMEM (\code{EVID: 2}). It cannot work with 
\code{HOUR} and \code{DATETIME} simultaneously, so in mixed cases, \code{plus.rigged} tries 
both sequentially and combines the result.

\code{as.moot} and \code{as.rigged} return their arguments as a list classified as \code{moot}
or \code{rigged}, respectively. In operator context, \code{- as.moot} or \code{+ as.rigged}
cause \code{minus.moot} or \code{plus.rigged} to be called. These call \code{moot} and \code{rig}
with the left operand as \code{x} and the right operand as a list of extra arguments;
then they do appropriate processing.  \code{minus.moot} drops moot rows; \code{plus.rigged}
merges the rigged data.frame.

\code{deranged} and \code{moot} can be used together with effect.  \code{deranged} creates 
explicit rows that can be merged by \code{TIME} to existing data.  After the merge,
it will be apparent that many such rows are moot. Functions here support 
dynamic removal.  See example for \code{as.nm}.
}
\value{
\code{moot.nm} returns logical, with length \code{nrow(x)}. \code{as.moot} returns a list of 
class \code{moot}. \code{minus.moot} returns a subset of its first argument, typically nm.

\code{rig.nm} returns a data.frame of rows evenly spaced across each subject's 
timecourse (i.e., \sQuote{rigging}). \code{as.rigged} returns a list of class \code{rigged}.
\code{plus.rigged} returns a merge on its first argument, typically nm.

\code{deranged} returns keyed data.frame.

}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.nm}}
		\item \code{\link{Ops.keyed}}
	}
}
\keyword{manip}


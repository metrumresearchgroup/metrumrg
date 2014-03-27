\name{deranged}
\alias{deranged.data.frame}
\alias{deranged.keyed}
\alias{deranged}

\title{Instantiate Records Implied By Range Data}
\description{
\code{deranged} generates the records implied by the range stored as \code{start} and \code{stop}.
}
\usage{
\method{deranged}{keyed}(x, ...)
\method{deranged}{data.frame}(x, start, stop, result = start, dropStop = TRUE, ...)
}
\arguments{
  \item{x}{dispatch argument}
  \item{start}{name of column with \code{start} values}
  \item{stop}{name of column with \code{stop} values}
  \item{result}{name of column in which to place the result}
  \item{dropStop}{whether to drop the \code{stop} column before returning}
  \item{\dots}{extra arguments, usually ignored; see details}  
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
}
\value{
\code{deranged} returns keyed data.frame.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.nm}}
		\item \code{\link{Ops.keyed}}
	}
}
\keyword{manip}


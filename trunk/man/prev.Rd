\name{prev}
\alias{maxChar}
\alias{prev}
\alias{nxt}
\alias{runhead}
\title{Check Some Properties of Vectors}
\description{
	\code{maxChar} returns the number of printed characters for the widest element of \code{x}.
	\code{prev} calculates the previous element for each element in \code{x}.
	\code{nxt} calculates the next element for each element in \code{x}.
	\code{runhead} determines whether each element is the \sQuote{head} (start) of a run.
}
\usage{
maxChar(x)
prev(x)
nxt(x)
runhead(x)
}
\arguments{
	\item{x}{vector}
}
\details{
	If you forget to \code{round} or \code{signif} a numeric column, you may get more digits
	than desired in your output file; \code{maxChar} can warn you.  See examples.

	\code{prev} is used by \code{runhead}. Note that there is no element previous to the first
	element in a vector, so \code{prev} returns \code{NA} in that position. \code{nxt} is
	the reverse of \code{prev} (literally).  Note that \code{next} is a reserved
	language word in \R.

	If a \sQuote{run} is a sequence (possibly of length one) of identical successive values
	in a vector, \code{runhead} determines whether an element is the first in such a sequence.
	Note that by definition, the first element is the start of a run; thus \code{runhead}
	returns \code{TRUE} in that position, even though \code{prev} returns \code{NA}.
}
\value{
	\code{maxChar}: a scalar integer
	\code{prev}: a vector of the same class
	\code{nxt}: a vector of the same class
	\code{runhead}: a vector of logicals
}
\author{Tim Bergsma}
\note{
\code{NA}s in the argument to \code{runhead} give surprising but reasonable results. It cannot
be known whether they are the heads of runs, nor can it be known whether values
immediately following them are heads of runs.  To treat \code{NA}s deterministically,
convert to some definite value first.
}
\examples{
maxChar(c(1.2,1.234))
prev(c(1,2,NA,3,3,NA,4))
nxt(c(1,2,NA,3,3,NA,4))
runhead(c(1,2,NA,3,3,NA,4))
}
\keyword{manip}


\name{Ops.keyed}
\alias{Ops.keyed}
\alias{and.keyed}
\alias{left.keyed}
\alias{minus.keyed}
\alias{plus.keyed}

\title{Operator Syntax for Joining Keyed Data Frames}
\description{
These functions implement a concise syntax for joining objects of class \code{c('keyed', 'data frame')}.
\code{+} produces an outer join, \code{&} produces an inner join, and \code{|} gives
a left join.  By default, \code{-} drops rows in \code{x}
that have matching rows in \code{y}. The syntax can be extended to subclasses of
\code{keyed} by writing additional proxy methods.
}
\usage{
\method{Ops}{keyed}(e1, e2)
\method{plus}{keyed}(x, y)
\method{and}{keyed}(x, y)
\method{minus}{keyed}(x, y)
\method{left}{keyed}(x, y)
}
\arguments{
  \item{e1}{left argument to Ops}
  \item{e2}{right argument to Ops}
  \item{x}{left argument to proxies}
  \item{y}{right argument to proxies}
  
  
}
\details{
A concise syntax for joining of data.frames facilitates dynamic assembly of data.
This system leverages existing operators and dispatch mechanisms.  Under \code{Ops} 
dispatch rules, if both left and right operands resolve to the same the method,
that method is used.  Operator methods are already defined for data.frame, but
the existence of class \sQuote{keyed} creates an opportunity for syntax specification.

\code{Ops.keyed} selects a \dfn{proxy} method, and dispatches on the right hand operand.  
Proxy methods are text equivalents of simple operators, e.g. the proxy \code{plus} 
corresponds to \code{+}.  They are necessary because direct specification of, say,
\code{+.keyed} could not be overridden for subclasses of \code{keyed} (\code{Ops} mechanism would
detect conflicting methods for the two operands, and would default to primitives.)

Dispatch on the right hand operand is consistent with a general syntax of data
assembly.  For \code{a + b + c ...}, results accumulate on the left under a somewhat
fixed typology (class), and the next operand on the right controls the next
step of assembly, perhaps invoking a specialized method by means of its class 
(S3 dispatch mechanism).

Operators have been chosen to coordinate intuition about their effects with 
existing operator precedence and a general data assembly pattern. \code{+} gives
an outer join (\code{merge}, with \code{all=TRUE}). \code{&} gives an inner join (\code{all=FALSE}): 
mnemonically, rows must match in the left AND right operands to contribute to
the result.  \code{-} suggests removal: it is used for methods that drop rows. \code{|}
gives a left join (\code{merge}, \code{all.x=TRUE}). Mnemonically, it suggests conditioning (as
with formulas): use of rows on the right is conditional on existence of matches on the left.
Right joins are currently not implemented, but most can be expressed as left joins by 
rearrangement.

Operator pair \code{+} and \code{-} has higher precedence than the pair \code{&} and \code{|}.
Within pairs, operators have equal precedence and resolve left to right (see ?S3groupGeneric). 
A common assembly sequence is one or more outer joins followed by one or more 
left joins. Correspondence to the existing order of operations minimizes the 
need for parenthetical grouping of terms (which is available nonetheless).

Proxy methods have been defined on these operators for class \sQuote{keyed}.  Users
can implement differing assembly operations by creating additional classes
and defining proxy methods for them.  \code{plus.rigged} and \code{minus.moot} are 
examples.  See examples for \code{as.nm}.
}
\value{keyed data.frame}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.nm}}
	}
}
\keyword{manip}


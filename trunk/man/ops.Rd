\name{Ops.keyed}
\alias{Ops.keyed}
\alias{and.keyed}
\alias{left.keyed}
\alias{minus.keyed}
\alias{plus.keyed}
\alias{raised.keyed}
\alias{times.keyed}
\alias{divide.keyed}
\alias{not.keyed}

\title{Operator Syntax for Joining Keyed Data Frames}
\description{
These functions implement a concise syntax for joining objects of class \code{c('keyed', 'data frame')}.
\code{+} produces an outer join, \code{&} produces an inner join, and \code{|} gives
a left join. \code{*} produces a column-stable outer join (same columns and order as for \code{x}).

By default, \code{-} drops rows in \code{x} that have matching rows in \code{y}. 
\code{/} drops rows in \code{x} that \emph{do not} have matching rows in \code{y}.

\code{^} gives a serial left join: x is joined cumulatively with static variants of y,
using left subsets of the key (e.g. subj, subj-time, subj-time-cmt, etc.).

\code{!}, a unary operator, returns rows in x that have NA keys or duplicate/duplicated keys.

The syntax can be extended to subclasses of
\code{keyed} by writing additional proxy methods.
}
\usage{
\method{Ops}{keyed}(e1, e2)
plus.keyed(x, y)
times.keyed(x, y)
and.keyed(x, y)
minus.keyed(x, y)
divide.keyed(x, y)
left.keyed(x, y)
raised.keyed(x, y)
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
an outer join (\code{merge}, with \code{all=TRUE}). \code{*} further restricts to 
columns on the left (\emph{star} suggests \emph{stable}).  \code{&} gives an inner join (\code{all=FALSE}): 
mnemonically, rows must match in the left AND right operands to contribute to
the result.  \code{-} suggests removal: it is used for methods that drop rows. \code{|}
gives a left join (\code{merge}, \code{all.x=TRUE}). Mnemonically, it suggests conditioning (as
with formulas): use of rows on the right is conditional on existence of matches on the left.
Right joins are currently not implemented, but most can be expressed as left joins by 
rearrangement. \code{!} invokes the proxy \code{not.keyed}, which is the best mnemonic: rows are returned
that have problematic keys (NA or duplicates), i.e. ``not keyed'' correctly.

Operators \code{+,*,/} and \code{-} have higher precedence than \code{&,|} and \code{!}.
Within groups, operators have equal precedence and resolve left to right (see \code{?S3groupGeneric}). 
A common assembly sequence is one or more full joins followed by one or more 
left joins. Correspondence to the existing order of operations minimizes the 
need for parenthetical grouping of terms (which is available nonetheless).

Proxy methods have been defined on these operators for class \sQuote{keyed}.  Users
can implement differing assembly operations by creating additional classes
and defining proxy methods for them.  \code{plus.rigged} and \code{minus.moot} are 
examples.  See examples for \code{as.nm}.
}
\value{keyed data.frame}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.nm}}
		\item \code{\link{static.keyed}}
	}
}
\examples{
dose <- data.frame( 
	SUBJ = rep(letters[1:3], each = 2), 
	HOUR = rep(c(0,20),3), 
	AMT = rep(c(40,60,80), each = 2) 
) 
dose <- as.keyed(dose,key=c('SUBJ','HOUR'))
samp <- data.frame( 
	SUBJ = rep(letters[1:3], each = 4), 
	HOUR = rep(c(0,10,20,30),3), 
	DV = signif(rnorm(12),2) + 2 
) 
samp <- as.keyed(samp,key=c('SUBJ','HOUR'))
demo <- data.frame( 
	SUBJ = letters[2:5], 
	RACE = c('asian','white','black','other'), 
	SEX = c('female','male','female','male'), 
	WT = c(75, 70, 73, 68) 
)
demo <- as.keyed(demo,key=c('SUBJ'))
meds <- as.keyed(
	data.frame(
		SUBJ=c('a','c'),
		HOUR=c(0,15),
		STOP=c(10,25),
		C3A4=as.flag(c(1,1))
	),
	key=c('SUBJ','HOUR')
)

dose + samp
dose * samp
dose & samp
samp - dose
samp / dose
dose | demo
demo | dose
demo ^ dose

a <- data.frame(
  subj=c(1,1,1,2,2,2,3,3,3),
  time=c(0,1,2,0,1,2,0,1,2),
  conc=c(1,2,3,1,2,3,1,2,3)
)
a <- as.keyed(a,c('subj','time'))

b <-data.frame(
  subj=c(1,1,2,2,3,3),
  time=c(0,2,0,2,0,2),
  conc=c(1,3,1,3,1,3),
  crcl=c(5,5,6,6,7,7),
  pred=c(2,4,2,4,2,4)
)
b <- as.keyed(b,c('subj','time'))
a|b
a^b # note imputation of apparently-constant crcl
	
}
\keyword{manip}


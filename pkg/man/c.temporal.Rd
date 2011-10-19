\name{c.temporal}
\alias{c.temporal}
\alias{[.temporal}
\alias{[<-.temporal}
\alias{[[.temporal}
\alias{as.character.temporal}
\alias{as.chartime.numeric}
\alias{as.chartime}
\alias{as.numeric.chartime}
\alias{print.temporal}
\alias{seq.temporal}
\alias{rep.temporal}
\title{Support for temporal Classes}
\description{
  These functions support classes temporal, mTime, mDate, and mDateTime
  (and related functions).  They are mostly S3 methods for base R generics.
}
\usage{
\method{c}{temporal}(..., recursive = FALSE)
\method{[}{temporal}(x, ..., drop = TRUE)
\method{[}{temporal}(x, ...) <- value
\method{[[}{temporal}(x, ..., drop = TRUE)
\method{as.character}{temporal}(x, ...)
as.chartime(x, ...)
\method{as.chartime}{numeric}(x, format, mark=TRUE,...)
\method{as.numeric}{chartime}(x, format,...)
\method{print}{temporal}(x, ...)
\method{rep}{temporal}(x, ...)
\method{seq}{temporal}(from, to, by = NULL, length.out = NULL, along.with = NULL, ...)
}

\arguments{
  \item{\dots}{arguments to \code{c}, or passed to other functions}
  \item{recursive}{same meaning as for \code{c}}
  \item{x}{object of class temporal}
  \item{drop}{same meaning as for \sQuote{[} and \sQuote{[[}}
  \item{value}{value to be assigned, as for \sQuote{[<-}}
  \item{format}{input or output format describing character time (see \code{\link{strftime}})}
  \item{mark}{boolean: mark times with dangling seconds using \sQuote{+}}
  \item{from}{as for \code{\link{seq.default}}}
  \item{to}{as for \code{\link{seq.default}}}
  \item{by}{as for \code{\link{seq.default}}}
  \item{length.out}{as for \code{\link{seq.default}}}
  \item{along.with}{as for \code{\link{seq.default}}}
}
\details{
  Normally you shouldn't need to worry about these functions.  \code{c} and the \sQuote{[} 
  variants exist just so that class information is not lost on invocation of the 
  generic.  \code{as.character.temporal} and \code{print.temporal} just call \code{format}.
  \code{chartime} variants are used internally by other functions.  \code{seq.temporal} 
  requires \code{from} and \code{to}, sets smart defaults for \code{by} (if \code{null}), and preserves
  class information, relative to \code{seq.default}.
}
\value{
	\item{print}{an invisible object with same class as \code{x}}
  \item{as.chartime}{generic: does not return}
  \item{as.chartime.numeric}{character (time)}
  \item{as.numeric.chartime}{numeric (seconds)}
  \item{as.character.temporal}{character (time)}
  \item{others}{object with same class as \code{x}}
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{temporal}} 
		\item \code{\link{seq.default}}
		\item \code{\link{strftime}}
	}
}
\examples{

#as.data.frame
data.frame(
	dt=as.mDateTime(seq(from=0,by=86500,length.out=3)),
	d=as.mDate(seq(from=0,by=86400,length.out=3)),
	t=as.mTime(c(60,120,180))
)
#                  dt          d     t
# 1  1970-01-01 00:00 1970-01-01 00:01
# 2 1970-01-02 00:01+ 1970-01-02 00:02
# 3 1970-01-03 00:03+ 1970-01-03 00:03

#combine
c(as.mTime(0),as.mTime(60))
# 00:00 00:01
c(as.mDate(0),as.mDate(86400))
# 1970-01-01 1970-01-02
c(as.mDateTime(0),as.mDateTime(86500))
# 1970-01-01 00:00  1970-01-02 00:01+

#subset
as.mTime(c('08:00','09:00'))[2]
# 09:00
as.mDate(c('2008-01-01','2008-01-04'))[2]
# 2008-01-04
as.mDateTime(c('2008-01-01 12:00','2008-01-04 12:30'))[2]
# 2008-01-04 12:30

#element selection
as.mTime(c('08:00','09:00'))[[2]]
# 09:00
as.mDate(c('2008-01-01','2008-01-04'))[[2]]
# 2008-01-04
as.mDateTime(c('2008-01-01 12:00','2008-01-04 12:30'))[[2]]
# 2008-01-04 12:30

#assignment
a <- as.mTime(seq(60,300, by=60))
a#00:01 00:02 00:03 00:04 00:05
a[5] <- 60
a#00:01 00:02 00:03 00:04 00:01
a[3] <- NA
a#00:01 00:02 <NA>  00:04 00:01

#identity
as.mTime(as.mTime(0))
# 00:00
as.mDate(as.mDate(0))
# 1970-01-01
as.mDateTime(as.mDateTime(0))
# 1970-01-01 00:00

#repetition
rep(as.mTime(86340),2)
# 23:59 23:59
}
\keyword{manip}

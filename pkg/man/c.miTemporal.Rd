\name{c.miTemporal}
\alias{c.miTemporal}
\alias{[.miTemporal}
\alias{[<-.miTemporal}
\alias{[[.miTemporal}
\alias{as.character.miTemporal}
\alias{as.chartime.numeric}
\alias{as.chartime}
\alias{as.numeric.chartime}
\alias{print.miTemporal}
\alias{seq.miTemporal}
\alias{rep.miTemporal}
\title{Support for miTemporal Classes}
\description{
  These functions support classes miTemporal, miTime, miDate, and miDateTime
  (and related functions).  They are mostly S3 methods for base R generics.
}
\usage{
\method{c}{miTemporal}(..., recursive = FALSE)
\method{[}{miTemporal}(x, ..., drop = TRUE)
\method{[}{miTemporal}(x, ...) <- value
\method{[[}{miTemporal}(x, ..., drop = TRUE)
\method{as.character}{miTemporal}(x, ...)
as.chartime(x, ...)
\method{as.chartime}{numeric}(x, format, mark=TRUE,...)
\method{as.numeric}{chartime}(x, format,...)
\method{print}{miTemporal}(x, ...)
\method{rep}{miTemporal}(x, ...)
\method{seq}{miTemporal}(from, to, by = NULL, length.out = NULL, along.with = NULL, ...)
}

\arguments{
  \item{\dots}{arguments to \code{c}, or passed to other functions}
  \item{recursive}{same meaning as for \code{c}}
  \item{x}{object of class miTemporal}
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
  generic.  \code{as.character.miTemporal} and \code{print.miTemporal} just call \code{format}.
  \code{chartime} variants are used internally by other functions.  \code{seq.miTemporal} 
  requires \code{from} and \code{to}, sets smart defaults for \code{by} (if \code{null}), and preserves
  class information, relative to \code{seq.default}.
}
\value{
	\item{print}{an invisible object with same class as \code{x}}
  \item{as.chartime}{generic: does not return}
  \item{as.chartime.numeric}{character (time)}
  \item{as.numeric.chartime}{numeric (seconds)}
  \item{as.character.miTemporal}{character (time)}
  \item{others}{object with same class as \code{x}}
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{miTemporal}} 
		\item \code{\link{seq.default}}
		\item \code{\link{strftime}}
	}
}
\examples{

#as.data.frame
data.frame(
	dt=as.miDateTime(seq(from=0,by=86500,length.out=3)),
	d=as.miDate(seq(from=0,by=86400,length.out=3)),
	t=as.miTime(c(60,120,180))
)
#                  dt          d     t
# 1  1970-01-01 00:00 1970-01-01 00:01
# 2 1970-01-02 00:01+ 1970-01-02 00:02
# 3 1970-01-03 00:03+ 1970-01-03 00:03

#combine
c(as.miTime(0),as.miTime(60))
# 00:00 00:01
c(as.miDate(0),as.miDate(86400))
# 1970-01-01 1970-01-02
c(as.miDateTime(0),as.miDateTime(86500))
# 1970-01-01 00:00  1970-01-02 00:01+

#subset
as.miTime(c('08:00','09:00'))[2]
# 09:00
as.miDate(c('2008-01-01','2008-01-04'))[2]
# 2008-01-04
as.miDateTime(c('2008-01-01 12:00','2008-01-04 12:30'))[2]
# 2008-01-04 12:30

#element selection
as.miTime(c('08:00','09:00'))[[2]]
# 09:00
as.miDate(c('2008-01-01','2008-01-04'))[[2]]
# 2008-01-04
as.miDateTime(c('2008-01-01 12:00','2008-01-04 12:30'))[[2]]
# 2008-01-04 12:30

#assignment
a <- as.miTime(seq(60,300, by=60))
a#00:01 00:02 00:03 00:04 00:05
a[5] <- 60
a#00:01 00:02 00:03 00:04 00:01
a[3] <- NA
a#00:01 00:02 <NA>  00:04 00:01

#identity
as.miTime(as.miTime(0))
# 00:00
as.miDate(as.miDate(0))
# 1970-01-01
as.miDateTime(as.miDateTime(0))
# 1970-01-01 00:00

#repetition
rep(as.miTime(86340),2)
# 23:59 23:59
}
\keyword{manip}

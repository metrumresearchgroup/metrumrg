\name{temporal}
\alias{temporal}
\alias{as.mTime}
\alias{as.mTime.character}
\alias{as.mTime.numeric}
\alias{as.mTime.mTime}
\alias{as.mDate.character}
\alias{as.mDate}
\alias{as.mDate.numeric}
\alias{as.mDate.mDate}
\alias{as.mDateTime}
\alias{as.mDateTime.character}
\alias{as.mDateTime.numeric}
\alias{as.mDateTime.mDate}
\alias{as.mDateTime.mDateTime}
\alias{format.mTime}
\alias{format.mDate}
\alias{format.mDateTime}
\alias{xtfrm.temporal}
\alias{as.mDate.Date}
\alias{as.mDate.dates}
\alias{as.mDateTime.POSIXct}
\alias{as.mDateTime.POSIXlt}
\alias{as.mDateTime.chron}
\alias{as.mTime.times}
\alias{unique.temporal}

\title{Temporal Classes with Selective Defaults}
\description{
  \code{temporal} is an abstract superclass of \code{mTime}, \code{mDate},
  and \code{mDateTime}.  These latter are convenience classes that store temporal
  information as seconds since the start of 1970-01-01.  They rely on \code{POSIXlt} and
  \code{POSIXct}, giving full access to the format constructs of \code{\link{strftime}}.
  However, the concepts of \sQuote{origin} and \sQuote{timezone} are deconstructed (fixed to 
  1970-01-01 and GMT).  Default formats are suitably chosen for inputs 
  (\code{as.character} methods) and outputs (\code{format} methods) and may be overridden. 
  By default, \code{format} will append a \sQuote{+} symbol to temporals with dangling seconds 
  (not multiples of 60): seconds are not displayed by default but still operate 
  (perhaps dangerously) in comparisons.
}
\usage{
as.mTime(x, ...)
\method{as.mTime}{character}(x, format = '\%H:\%M',...)
\method{as.mTime}{numeric}(x,...)
\method{as.mTime}{mTime}(x, ...)
\method{as.mTime}{times}(x, ...)
as.mDate(x, ...)
\method{as.mDate}{character}(x, format = '\%Y-\%m-\%d',...)
\method{as.mDate}{numeric}(x,...)
\method{as.mDate}{Date}(x,...)
\method{as.mDate}{mDate}(x,...)
\method{as.mDate}{dates}(x,...)
as.mDateTime(x, ...)
\method{as.mDateTime}{character}(x, format = '\%Y-\%m-\%d \%H:\%M',...)
\method{as.mDateTime}{numeric}(x,...)
\method{as.mDateTime}{mDate}(x, y = 0,...)
\method{as.mDateTime}{mDateTime}(x, ...)
\method{as.mDateTime}{POSIXct}(x, ...)
\method{as.mDateTime}{POSIXlt}(x, ...)
\method{as.mDateTime}{chron}(x, ...)
\method{format}{mTime}(x, format = '\%H:\%M', mark=TRUE,...)
\method{format}{mDate}(x, format = '\%Y-\%m-\%d', mark=TRUE,...)
\method{format}{mDateTime}(x, format = '\%Y-\%m-\%d \%H:\%M', mark=TRUE,...)
\method{unique}{temporal}(x, incomparables=FALSE,...)
\method{xtfrm}{temporal}(x,...)
}

\arguments{
  \item{x}{character time as per format, numeric seconds since 1970-01-01, 
  	or temporal subclass}
  \item{\dots}{other arguments, usually ignored}
  \item{y}{optional time for constructing mDateTime from mDate (defaults to 
  	midnight}
  \item{format}{character, as per \code{\link{strftime}}}
  \item{mark}{boolean: mark times with dangling seconds using \sQuote{+}}
  \item{incomparables}{passed to \code{unique}}
 }
\details{
  Creating a temporal object with these methods ultimately calls one of the 
  \code{.numeric} methods, each of which round their first argument to zero
  places.  This means that all comparisons are based on whole numbers, and 
  therefore not subject to rounding errors.  
  
  Seconds that are not multiples of 60 can be stored in mTime and mDateTime 
  objects, but will not be displayed by default (see above).  mDate can only 
  store numbers of seconds that correspond to midnight, so adding an mDate and 
  mTime gives a predictable result.  
  
  The temporal classes are all subclasses of numeric, so numeric methods are
  available (but may not preserve class information; try \code{max} for example).
  
  temporal classes support \code{NA}, \code{Inf}, \code{-Inf}, \code{as.data.frame}, \code{seq}, subset, element
  selection, element assignment, and interconversion.
}
\value{
  \item{format}{character}
  \item{as.mTime}{object with class \code{c('mTime','temporal','numeric')}}
  \item{as.mDate}{object with class \code{c('mDate','temporal','numeric')}}
  \item{as.mDateTime}{object with class \code{c('mDateTime','temporal','numeric')}}
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{c.temporal}}
	}
}
\examples{
#numeric to temporal
as.mTime(0)
# 00:00
as.mTime(1)
# 00:00+
as.mTime(-1)
# 23:59+
as.mTime(60)
# 00:01
as.mTime(-60)
# 23:59
as.mTime(86400)
# 00:00
as.mTime(86460)
# 00:01
as.mDate(0)
# 1970-01-01
as.mDate(1)
# 1970-01-01
as.mDate(-1)
# 1969-12-31
as.mDate(-86400)
# 1969-12-31
as.mDate(-86401)
# 1969-12-30
as.mDateTime(0)
# 1970-01-01 00:00
as.mDateTime(60)
# 1970-01-01 00:01 
as.mDateTime(61)
# 1970-01-01 00:01+
as.mDateTime(-1)
# 1969-12-31 23:59+

#character to temporal
as.mTime('00:00')
# 00:00
as.mTime('23:59')
# 23:59
as.mTime('23:59:00')
# 23:59
as.mTime('23:59:01')
# 23:59
as.mTime('23:59:01',format='\%H:\%M:\%S')
# 23:59+
as.mTime('24:00')
# NA
as.mDate('1970-01-02')
# 1970-01-02
as.mDate('01/02/1970',format='\%m/\%d/\%Y')
# 1970-01-02
as.mDateTime('01/02/1970 12:30',format='\%m/\%d/\%Y \%H:\%M')
# 1970-01-02 12:30
as.mDateTime('01/02/1970 12:30:15',format='\%m/\%d/\%Y \%H:\%M:\%S')
# 1970-01-02 12:30+

#temporal to numeric
as.numeric(as.mTime(0))
# 0
as.numeric(as.mTime(1))
# 1
as.numeric(as.mTime(-1))
# 86399
as.numeric(as.mTime(60))
# 60
as.numeric(as.mTime(-60))
# 86340
as.numeric(as.mTime(86400))
# 0
as.numeric(as.mTime(86460))
# 60
as.numeric(as.mDate(0))
# 0
as.numeric(as.mDate(1))
# 0
as.numeric(as.mDate(-1))
# -86400
as.numeric(as.mDate(-86400))
# -86400
as.numeric(as.mDate(-86401))
# -172800
as.numeric(as.mDateTime(0))
# 0
as.numeric(as.mDateTime(60))
# 60 
as.numeric(as.mDateTime(61))
# 61
as.numeric(as.mDateTime(-1))
# -1
as.numeric(as.mTime('00:00'))
# 0
as.numeric(as.mTime('23:59'))
# 86340
as.numeric(as.mTime('23:59:00'))
# 86340
as.numeric(as.mTime('23:59:01'))
# 86340
as.numeric(as.mTime('23:59:01',format='\%H:\%M:\%S'))
# 86341
as.numeric(as.mTime('24:00'))
# NA
as.numeric(as.mDate('1970-01-02'))
# 86400
as.numeric(as.mDate('01/02/1970',format='\%m/\%d/\%Y'))
# 86400
as.numeric(as.mDateTime('01/02/1970 12:30',format='\%m/\%d/\%Y \%H:\%M'))
# 131400
as.numeric(as.mDateTime('01/02/1970 12:30:15',format='\%m/\%d/\%Y \%H:\%M:\%S'))
# 131415

#temporal to character
as.character(as.mTime(0))
# '00:00'
as.character(as.mDate(0))
# '1970-01-01'
as.character(as.mDateTime(0))
# '1970-01-01 00:00'

#non-default printout
format(as.mTime(30000),format='\%H')
# '08'
format(as.mDate('1970-01-01'),format='\%d\%b\%y')
# '01Jan70'
format(as.mDateTime('1970-01-02 23:30'),format='[\%d/\%m/\%y \%H:\%M:\%S]')
# '[02/01/70 23:30:00]'
format(as.mTime(1))
# '00:00+'
format(as.mTime(1),mark=FALSE)
# '00:00'

#sequence
seq(from=as.mTime('8:00'),to=as.mTime('12:00'))
# 08:00 09:00 10:00 11:00 12:00
seq(from=as.mDate('2008-01-01'),to=as.mDate('2008-01-04'))
# 2008-01-01 2008-01-02 2008-01-03 2008-01-04
seq(from=as.mDateTime('2008-01-01 12:00'),to=as.mDateTime('2008-01-04 12:30'))
# 2008-01-01 12:00 2008-01-02 12:00 2008-01-03 12:00 2008-01-04 12:00

#interconversion
as.mTime(as.mDate('2008-10-14'))
# 00:00
as.mTime(as.mDateTime('2008-10-14 08:00'))
# 08:00
as.mDate(as.mTime('23:59'))
# 1970-01-01
as.mDate(as.mDateTime('2008-10-14 08:00'))
# 2008-10-14
as.mDateTime(as.mTime(6000000))
# 1970-01-01 10:40
as.mDateTime(as.mDate('2008-10-14'))
# 2008-10-14 00:00

#intercoversion from other systems
as.mDate(as.Date('1970-01-01'))
# 1970-01-01
as.mDateTime(as.POSIXct('1970-01-01',tz='GMT'))
# 1970-01-01 00:00
as.mDateTime(as.POSIXlt('1970-01-01',tz='GMT'))
# 1970-01-01 00:00
library(chron)
as.mTime(times('12:30:00'))
# 12:30
as.mDate(dates('01/01/1970'))
# 1970-01-01
as.mDateTime(chron(dates='01/01/1970',times='12:30:00'))
# 1970-01-01 12:30

#NA
as.mTime(as.numeric(NA))
# <NA>

#infinity
as.mTime(Inf)
# Inf
as.mDate(Inf)
# Inf
as.mDateTime(Inf)
# Inf
as.mTime(-Inf)
# -Inf
as.mDateTime(Inf) + as.mTime(Inf)
# Inf
as.mDateTime(Inf) + as.mTime(-Inf)
# <NA>

#comparison
as.mTime('08:00') < as.mTime('09:00')
# TRUE
as.mDate('1970-01-01') > as.mDate('2008-01-01')
# FALSE
as.mDateTime('1970-01-01 08:00') > as.mDate('1970-01-01')
# TRUE

#statistics
max(as.mDate(c('1970-01-01','1980-01-01','1975-01-01')))
# 315532800

#operations
as.mDateTime(0) + as.mTime(60)
# 1970-01-01 00:01
as.mTime(60) + as.mDateTime(0)
# 00:01

#unary operations
-as.mTime(1)
# 23:59+

#sorting
sort(unique(as.mTime(c(180,120,60))))
# 00:01 00:02 00:03
}
\keyword{manip}

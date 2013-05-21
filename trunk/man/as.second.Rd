\name{as.second}
\alias{duration}
\alias{as.second}
\alias{as.minute}
\alias{as.hour}
\alias{as.day}
\alias{as.week}
\alias{as.month}
\alias{as.year}
\alias{as.second.numeric}
\alias{as.minute.numeric}
\alias{as.hour.numeric}
\alias{as.day.numeric}
\alias{as.week.numeric}
\alias{as.month.numeric}
\alias{as.year.numeric}
\alias{format.duration}
\alias{print.duration}
\alias{as.second.minute}
\alias{as.minute.second}
\alias{as.minute.hour}
\alias{as.hour.minute}
\alias{as.hour.day}
\alias{as.day.hour}
\alias{as.day.week}
\alias{as.week.day}
\alias{as.day.month}
\alias{as.month.day}
\alias{as.day.year}
\alias{as.year.day}
\alias{as.second.second}
\alias{as.minute.minute}
\alias{as.hour.hour}
\alias{as.day.day}
\alias{as.week.week}
\alias{as.month.month}
\alias{as.year.year}
\alias{as.second.hour}
\alias{as.second.day}
\alias{as.second.duration}
\alias{as.minute.duration}
\alias{as.hour.second}
\alias{as.hour.duration}
\alias{as.day.duration}                                     
\alias{as.week.duration}
\alias{as.month.duration}
\alias{as.year.duration}
\title{Interconvert Nominal Time Units}
\description{Perform standard conversions among various common time units.}
 \usage{
as.second(x, ...)
as.minute(x, ...)
as.hour(x, ...)
as.day(x, ...)
as.week(x, ...)
as.month(x, ...)
as.year(x, ...)
}
\arguments{
  \item{x}{numeric}
  \item{\dots}{ignored}
}
\details{
The functions listed above are generic.  Methods exist, either explictly or implicitly,
for objects classified to represent second, minute, hour, day, week, month, and year.
All these objects are subclasses of "duration"; \code{as.<n>.duration} serves wherever
explicit methods are omitted. For each generic, methods exist for class "numeric".  
For each class, methods exist for the generics "format" and "print".

The strategy for time unit conversion is to classify a numeric vector according to some
unit, and then coerce that object to some other class.  Thus, \code{as.day(7)} is
not particularly interesting, but \code{as.week(as.day(7))} yields 1. 

Conversions use 60 seconds per minute, 60 minutes per hour, 24 hours per day,
7 days per week, 28 days per month, and 365.25 days per year.  Currently, no other
relationships are specified.  Note that 12 nominal months does not make a full year.
This is experimental, and may change in future versions.

The duration classes are also subclasses of \code{timeline}, which exists to 
support addition and subtraction of durations and timepoints.  
See examples here, and at \code{\link{timeline}}. You cannot add two timepoints, nor can you subtract a timepoint. When one argument is a timepoint,
  the other is coerced using \code{as.second}, and the result is the timepoint class.
  For two durations, the second value is coerced to the class of the first, with a message, if necessary.
  Otherwise, if only one argument is a duration, the other is coerced to that class. Negative durations are allowed. 
}
\value{
an S3 "num" object with class \code{c(n, 'duration','timeline','numeric')}, where `n' is
"second", "minute", "hour", "day", "week", "month", or "year", as implied.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.mDateTime}}
		\item \code{\link{timeline}}
		\item \code{\link{c.timeline}}
	}
}
\examples{
as.year(as.month(12))
as.year(as.day(365.25))
as.second(as.year(1))
as.month(2) + as.week(2)
as.week(2) + as.month(1)
as.month(2) - as.week(2)
as.week(2) - as.month(1)
as.week(2) + 1
as.week(2) - 1
2 + as.week(1)
2 - as.week(1)
class(c(as.day(1),as.day(2)))
class(as.day(1:5)[3])
class(as.day(1:5)[[3]])
class(seq(from=as.day(2),to=as.day(6)))
class(rep(as.day(1),5))
}
\keyword{manip}

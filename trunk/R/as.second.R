as.second         <- function(x,...)UseMethod('as.second')
as.minute         <- function(x,...)UseMethod('as.minute')
as.hour           <- function(x,...)UseMethod('as.hour')
as.day            <- function(x,...)UseMethod('as.day')
as.week           <- function(x,...)UseMethod('as.week')
as.month          <- function(x,...)UseMethod('as.month')
as.year           <- function(x,...)UseMethod('as.year')

as.second.numeric <- function(x,...)structure(x,class=c('second','nominal',class(x)))
as.minute.numeric <- function(x,...)structure(x,class=c('minute','nominal',class(x)))
as.hour.numeric   <- function(x,...)structure(x,class=c('hour','nominal',class(x)))
as.day.numeric    <- function(x,...)structure(x,class=c('day','nominal',class(x)))
as.week.numeric   <- function(x,...)structure(x,class=c('week','nominal',class(x)))
as.month.numeric  <- function(x,...)structure(x,class=c('month','nominal',class(x)))
as.year.numeric   <- function(x,...)structure(x,class=c('year','nominal',class(x)))

format.nominal <- function(x,...)as.numeric(x)
print.nominal <- function(x,...)print(format(x))

as.second.minute  <- function(x,...)as.second(as.numeric(x*60))
as.minute.second  <- function(x,...)as.minute(as.numeric(x/60))
as.minute.hour    <- function(x,...)as.minute(as.numeric(x*60))
as.hour.minute    <- function(x,...)as.hour(as.numeric(x/60))
as.hour.day       <- function(x,...)as.hour(as.numeric(x*24))
as.day.hour       <- function(x,...)as.day(as.numeric(x/24))
as.day.week       <- function(x,...)as.day(as.numeric(x*7))
as.week.day       <- function(x,...)as.week(as.numeric(x/7))
as.day.month      <- function(x,...)as.day(as.numeric(x*28))
as.month.day      <- function(x,...)as.month(as.numeric(x/28))
as.day.year       <- function(x,...)as.day(as.numeric(x*365.25))
as.year.day       <- function(x,...)as.year(as.numeric(x/365.25))

as.second.second  <- function(x,...)x
as.minute.minute  <- function(x,...)x
as.hour.hour      <- function(x,...)x
as.day.day        <- function(x,...)x
as.week.week      <- function(x,...)x
as.month.month    <- function(x,...)x
as.year.year      <- function(x,...)x
as.second.hour    <- function(x,...)as.second(as.minute(x))
as.second.day     <- function(x,...)as.second(as.hour(x))
as.second.nominal <- function(x,...)as.second(as.day(x))
as.minute.nominal <- function(x,...)as.minute(as.second(x))
as.hour.second    <- function(x,...)as.hour(as.minute(x))
as.hour.nominal   <- function(x,...)as.hour(as.day(x))                                       
as.day.nominal    <- function(x,...)as.day(as.hour(x))                                      
as.week.nominal   <- function(x,...)as.week(as.day(x))                                  
as.month.nominal  <- function(x,...)as.month(as.day(x))
as.year.nominal   <- function(x,...)as.year(as.day(x))


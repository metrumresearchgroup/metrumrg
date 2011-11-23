as.numeric.chartime <- function(x,format,...)as.numeric(unclass(as.POSIXct(strptime(x,format),tz='GMT')))
as.chartime <- function(x,...)UseMethod('as.chartime')
as.chartime.numeric <- function(x,format,mark=TRUE,...){
	y <- strftime(as.POSIXct(x,tz='GMT',origin='1970-01-01'),format=format,tz='GMT')
	y[is.infinite(x)] <- x[is.infinite(x)]
	z <- rep('',length(y))
	if(mark){
		s <- !is.na(x) & is.finite(x) & x%%60!=0
		z[s] <- '+'
	}
	y <- glue(y,z)
	y[is.na(x)] <- NA
	y
}
as.mTime <- function(x,...)UseMethod('as.mTime')
as.mTime.numeric <- function(x,...){
	x <- round(x)
	x[is.finite(x)] <- x[is.finite(x)]%%(60*60*24)
	structure(x, class = c('mTime','temporal','numeric'))
}
as.mTime.character <- function(x,format='%H:%M',...)as.mTime(as.numeric.chartime(x,format))

as.mDate <- function(x,...)UseMethod('as.mDate')
as.mDate.numeric <- function(x,...){
	x <- round(x)
	f <- is.finite(x)
	x[f] <- x[f] - x[f]%%(60*60*24)
	structure(x, class = c('mDate','temporal','numeric'))
}
as.mDate.character <- function(x,format='%Y-%m-%d',...)as.mDate(as.numeric.chartime(x,format))
as.mDate.sasdate <-function(x,...)as.mDate(as.Date(x, origin="1960-01-01",...))
as.mDateTime <- function(x,...)UseMethod('as.mDateTime')
as.mDateTime.numeric <- function(x,...){
	x <- round(x)
	structure(x, class = c('mDateTime','temporal','numeric'))
}
as.mDateTime.character <- function(x,format='%Y-%m-%d %H:%M',...)as.mDateTime(as.numeric.chartime(x,format))
as.mDateTime.mDate <- function(x,y=0,...)as.mDateTime(as.numeric(x)+as.numeric(y))
format.mTime <- function(x,format='%H:%M',mark=TRUE,...)as.chartime(x,format,mark)
format.mDate <- function(x,format='%Y-%m-%d',mark=TRUE,...)as.chartime(x,format,mark)
format.mDateTime <- function(x,format='%Y-%m-%d %H:%M',mark=TRUE,...)as.chartime(x,format,mark)
as.character.temporal <- function(x,...)format(x,...)
print.temporal <-function(x,...){
	print(format(x,...),quote=FALSE)
	invisible(x)
}
c.temporal <- function (..., recursive = FALSE){
	args <- list(...)
	oldclass <- class(args[[1]])	
	structure(c(unlist(lapply(args, unclass))), class = oldclass)
}
seq.temporal <- function (from, to, by=NULL,length.out = NULL, along.with = NULL, ...){
	if(is.null(by))if(inherits(from,'mTime'))by=60*60
	if(is.null(by))if(inherits(from,'mDate'))by=60*60*24
	if(is.null(by))if(inherits(from,'mDateTime'))by=60*60*24
	x <- seq(
		from=as.numeric(from),
		to=as.numeric(to),
		by=by,
		...
	)
	class(x) <- class(from)
	x
}
as.mTime.mTime <- function(x,...)x
as.mDate.mDate <- function(x,...)x
as.mDateTime.mDateTime <- function(x,...)x
rep.temporal <- function (x, ...) structure(rep(as.numeric(x),...),class=class(x))


`[.temporal` <- function (x, ..., drop = TRUE)structure(NextMethod(.Generic), class = oldClass(x))
`[[.temporal` <- function (x, ..., drop = TRUE)structure(NextMethod(.Generic), class = oldClass(x))

`[<-.temporal` <- function (x, ..., value){
    if (!(length(value)))return(x)
    if(all(is.na(value)))value <- as.numeric(value)
    if(inherits(x,'mTime'))value <- as.mTime(value)
    if(inherits(x,'mDate'))value <- as.mDate(value)
    if(inherits(x,'mDateTime'))value <- as.mDateTime(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}
xtfrm.temporal <- function(x,...)as.numeric(x)
as.mDate.Date <- function(x,...)as.mDate(round(as.numeric(x))*86400)
as.mDateTime.POSIXct <- function(x,...)as.mDateTime(round(as.numeric(x)))
as.mDateTime.POSIXlt <- function(x,...)as.mDateTime(as.POSIXct(x))
as.mTime.times <- function(x,...)as.mTime(as.numeric(x)*86400)
as.mDate.dates <- function(x,...)as.mDate(as.numeric(x)*86400)
as.mDateTime.chron <- function(x,...)as.mDateTime(as.numeric(x)*86400)
unique.temporal <- function(x, incomparables=FALSE,...)unique.numeric_version(x,incomparables,...)

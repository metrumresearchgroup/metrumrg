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
as.miTime <- function(x,...)UseMethod('as.miTime')
as.miTime.numeric <- function(x,...){
	x <- round(x)
	x[is.finite(x)] <- x[is.finite(x)]%%(60*60*24)
	structure(x, class = c('miTime','miTemporal','numeric'))
}
as.miTime.character <- function(x,format='%H:%M',...)as.miTime(as.numeric.chartime(x,format))

as.miDate <- function(x,...)UseMethod('as.miDate')
as.miDate.numeric <- function(x,...){
	x <- round(x)
	f <- is.finite(x)
	x[f] <- x[f] - x[f]%%(60*60*24)
	structure(x, class = c('miDate','miTemporal','numeric'))
}
as.miDate.character <- function(x,format='%Y-%m-%d',...)as.miDate(as.numeric.chartime(x,format))

as.miDateTime <- function(x,...)UseMethod('as.miDateTime')
as.miDateTime.numeric <- function(x,...){
	x <- round(x)
	structure(x, class = c('miDateTime','miTemporal','numeric'))
}
as.miDateTime.character <- function(x,format='%Y-%m-%d %H:%M',...)as.miDateTime(as.numeric.chartime(x,format))
as.miDateTime.miDate <- function(x,y=0,...)as.miDateTime(as.numeric(x)+as.numeric(y))
format.miTime <- function(x,format='%H:%M',mark=TRUE,...)as.chartime(x,format,mark)
format.miDate <- function(x,format='%Y-%m-%d',mark=TRUE,...)as.chartime(x,format,mark)
format.miDateTime <- function(x,format='%Y-%m-%d %H:%M',mark=TRUE,...)as.chartime(x,format,mark)
as.character.miTemporal <- function(x,...)format(x,...)
print.miTemporal <-function(x,...){
	print(format(x,...),quote=FALSE)
	invisible(x)
}
c.miTemporal <- function (..., recursive = FALSE){
	args <- list(...)
	oldclass <- class(args[[1]])	
	structure(c(unlist(lapply(args, unclass))), class = oldclass)
}
seq.miTemporal <- function (from, to, by=NULL,length.out = NULL, along.with = NULL, ...){
	if(is.null(by))if(inherits(from,'miTime'))by=60*60
	if(is.null(by))if(inherits(from,'miDate'))by=60*60*24
	if(is.null(by))if(inherits(from,'miDateTime'))by=60*60*24
	x <- seq(
		from=as.numeric(from),
		to=as.numeric(to),
		by=by,
		...
	)
	class(x) <- class(from)
	x
}
as.miTime.miTime <- function(x,...)x
as.miDate.miDate <- function(x,...)x
as.miDateTime.miDateTime <- function(x,...)x
rep.miTemporal <- function (x, ...) structure(rep(as.numeric(x),...),class=class(x))


`[.miTemporal` <- function (x, ..., drop = TRUE)structure(NextMethod(.Generic), class = oldClass(x))
`[[.miTemporal` <- function (x, ..., drop = TRUE)structure(NextMethod(.Generic), class = oldClass(x))

`[<-.miTemporal` <- function (x, ..., value){
    if (!(length(value)))return(x)
    if(all(is.na(value)))value <- as.numeric(value)
    if(inherits(x,'miTime'))value <- as.miTime(value)
    if(inherits(x,'miDate'))value <- as.miDate(value)
    if(inherits(x,'miDateTime'))value <- as.miDateTime(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}
xtfrm.miTemporal <- function(x,...)as.numeric(x)
as.miDate.Date <- function(x,...)as.miDate(round(as.numeric(x))*86400)
as.miDateTime.POSIXct <- function(x,...)as.miDateTime(round(as.numeric(x)))
as.miDateTime.POSIXlt <- function(x,...)as.miDateTime(as.POSIXct(x))
as.miTime.times <- function(x,...)as.miTime(as.numeric(x)*86400)
as.miDate.dates <- function(x,...)as.miDate(as.numeric(x)*86400)
as.miDateTime.chron <- function(x,...)as.miDateTime(as.numeric(x)*86400)
unique.miTemporal <- function(x, incomparables=FALSE,...)unique.numeric_version(x,incomparables,...)

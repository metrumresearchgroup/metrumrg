`fixedwidth` <-
function(x,...)UseMethod('fixedwidth')
`fixedwidth.data.frame` <-
function(x,...){
	x <- data.frame(lapply(x,format,justify='right'),stringsAsFactors=FALSE)
	x <- rbind(names(x),x)
	x <- data.frame(lapply(x,format,justify='right'),stringsAsFactors=FALSE)
	names(x) <- x[1,]
	x[-1,]
}

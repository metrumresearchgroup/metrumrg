metaMerge <- function(x,...)UseMethod('metaMerge')
metaMerge.list <- function(x,groom=as.best,...){
	if(!length(x))return(x)
	groom <- match.fun(groom)
	x[[1]] <- groom(x[[1]],...)
	if(length(x)==1)return(x[[1]])
	metaMerge(all=TRUE,x=x[[1]],y=metaMerge(x[-1]),...)
}
metaMerge.character <- function(x,import=read.table,groom=as.best,...){
	miss <- x[!file.exists(x)]
	if(length(miss))stop('cannot find, e.g.,',miss[[1]])
	import <- match.fun(import)
	x <- lapply(x,import,...)
	metaMerge(x,groom=groom,...)
}
metaMerge.default <- function(x,y,...)merge(x,y,...)

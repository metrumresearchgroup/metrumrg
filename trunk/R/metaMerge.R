metaMerge <- function(x,...)UseMethod('metaMerge')
metaMerge.list <- function(x,...){
	if(length(x)==0)return(x)
	if(length(x)==1)return(x[[1]])
	metaMerge(x=metaMerge(x[-length(x)]),y=x[[length(x)]],all=TRUE,...)
}
metaMerge.character <- function(x,import=read.table,...){
	miss <- x[!file.exists(x)]
	if(length(miss))stop('cannot find, e.g.,',miss[[1]])
	import <- match.fun(import)
	x <- lapply(x,import,...)
	metaMerge(x,...)
}
metaMerge.default <- function(x,y,...)merge(x,y,...)

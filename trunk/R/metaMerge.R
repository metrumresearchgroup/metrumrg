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
metaMerge.data.frame <- function(x,y,...){
	if(is.null(y))warning('merging data.frame with NULL object')
	merge(x,y,...)
}
metaMerge.NULL <- function(x,y,...){
	warning('merging NULL object')
	merge(x,y,...)
}
		

metaMerge <- function(x,...)UseMethod('metaMerge')
metaMerge.list <- function(x,all=TRUE,...){
	if(length(x)==0)return(x)
	if(length(x)==1)return(x[[1]])
	metaMerge(x=metaMerge(x[-length(x)]),y=x[[length(x)]],all=all,...)
}
metaMerge.character <- function(x,import=read.table,all=TRUE,...){
	miss <- x[!file.exists(x)]
	if(length(miss))stop('cannot find, e.g.,',miss[[1]])
	import <- match.fun(import)
	x <- lapply(x,import,...)
	metaMerge(x,all=all,...)
}
metaMerge.default <- function(x,y,all=TRUE,...)merge(x,y,all=all,...)
metaMerge.data.frame <- function(x,y,all=TRUE...){
	if(is.null(y))warning('merging data.frame with NULL object')
	merge(x,y,all=all,...)
}
metaMerge.NULL <- function(x,y,all=TRUE,...){
	warning('merging NULL object')
	merge(x,y,all=all,...)
}
		

merge.list <- function(x,groom=as.best,...){
	if(!length(x))return(x)
	groom <- match.fun(groom)
	x[[1]] <- groom(x[[1]],...)
	if(length(x)==1)return(x[[1]])
	merge(all=TRUE,x=x[[1]],y=merge(x[-1]),...)
}
merge.character <- function(x,import=read.table,groom=as.best,...){
	miss <- x[!file.exists(x)]
	if(length(miss))stop('cannot find, e.g.,',miss[[1]])
	import <- match.fun(import)
	x <- lapply(x,import,...)
	merge(x,groom=groom,...)
}


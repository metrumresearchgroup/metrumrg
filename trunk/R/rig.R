`deranged` <- function(x,...)UseMethod("deranged")

`deranged.data.frame` <- function(x,start,stop,result=start,dropStop=TRUE,...){
	y <- do.call(
		rbind,
		lapply(
			split(x,1:nrow(x)),
			function(x,...){
				instances <- seq(from=x[[start]],to=x[[stop]])
				x <- x[rep(1,length(instances)),]
				x[[result]] <- instances
				if(dropStop)x[[stop]] <- NULL
				x			
			}
		)
	)
	rownames(y) <- NULL
	return(y)
}

`deranged.keyed` <- function(x,...){
	y <- NextMethod()
	key(y) <- key(x)
	class(y) <- class(x)
	y
}



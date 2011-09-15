`as.keyed` <- function(x,...)UseMethod("as.keyed")

`as.keyed.data.frame` <- function(
	x,
	key,
	...
){
	key(x) <- key
	if(!inherits(x,'keyed'))class(x) <- c("keyed",class(x))
	x
}
`dupKeys` <- function(x,...)UseMethod('dupKeys')
`dupKeys.default` <- function(x,...){
	if(!all(key(x) %in% names(x)))stop('nonexistent key field(s)')
	y <- x[,key(x),drop=FALSE]
	duplicated(y)|duplicated(y,fromLast=TRUE)
}

`key` <- function(x,...)attr(x,"key")

`key<-` <- function(x,value){
	attr(x,"key") <- value
	x
}

`merge.keyed` <- function(x,y,...){
	z <- NextMethod()
	class(z) <- class(x)
	key(z) <- key(x)
	z
}
`naKeys` <- function(x,...)UseMethod('naKeys')
`naKeys.default` <- function(x,...){
	if(!all(key(x) %in% names(x)))stop('nonexistent key field(s)')
	if(nrow(x)==0)return(logical(0))
	y <- sapply(
		key(x),
		function(key)is.na(x[[key]])
	)
	if(nrow(x)==1)dim(y) <- c(1,length(y))
	as.logical(apply(y,1,sum))
}

`print.keyed.summary` <- function(x,...){
	writeLines(paste(x$key,collapse='~'))
	writeLines(paste(x$naKeys,'NA keys'))
	writeLines(paste(x$dupKeys,'duplicate keys'))
	if(!x$sorted)writeLines('unsorted')
	if(length(setdiff(names(x),c('key','naKeys','dupKeys','sorted'))))writeLines('other attributes present')
}
	
`summary.keyed` <- function(object,...){
	x<-object
	z <- list()
	z$key <- key(x)
	z$naKeys <- sum(naKeys(x))
	z$dupKeys <- sum(dupKeys(x))
	z$sorted <- identical(x,sort(x))
	class(z) <- 'keyed.summary'
	z	
}

`uniKey` <- function(x,...)UseMethod("uniKey")

`uniKey.keyed` <- function(x,key=NULL,...){
	if(is.null(key))key <- key(x)
	do.call(paste,x[,key,drop=FALSE])
}

aggregate.keyed <- function(
	x,
	by=x[,setdiff(key(x),across),drop=FALSE],
	FUN,
	across=character(0),
	...
){
	if(length(setdiff(across,key(x))))stop("'across' must be a subset of key(x)")
	if(length(names(by)) != length(by))stop("all elements of 'by' should be named")
	if(any(sapply(by,length)!=nrow(x)))stop("all elements of 'by' should have same length as nrow(x)")
	x <- x[,setdiff(names(x),across)] #drop across, which should be present if in key(x)
	x <- x[,setdiff(names(x),names(by)),drop=FALSE]#replace cols with by if like-named
	by <- c(by,list(`_superkey`=rep(1,nrow(x)))) #guarantee a key
	x <- cbind(by,x)
	x <- as.keyed(x,names(by))
	if(!any(dupKeys(x))){
		key(x) <- setdiff(key(x),'_superkey')
		x <- x[,setdiff(names(x),'_superkey')]
		return(x)
	}
	unique <- x[!dupKeys(x),]
	dups <- x[dupKeys(x),]
	molten <- melt(dups,id.var=names(by),variable_name='aggregate.keyed.variable')
	frozen <- cast(molten,formula=...~aggregate.keyed.variable,fun=FUN,...)
	frozen <- as.keyed(frozen,names(by))
	#x <- merge(unique,frozen,all=TRUE)
	x <- as.keyed(rbind(unique,frozen),names(by))
	sort(x)
	x[['_superkey']] <- NULL
	key(x) <- setdiff(key(x),'_superkey')
	if(!length(key(x))){
		class(x) <- setdiff(class(x),'keyed')
		key(x) <- NULL
	}
	x
}
#a <- as.keyed(data.frame(variable=c(1,1),id=4:5,val=2:3),key='variable')
#aggregate(a,FUN=mean)
`sort.keyed` <- function(x,decreasing=FALSE,...){
	if(!inherits(x,"data.frame"))stop(
		"sort.keyed assumes x is data.frame"
	)
	if(!length(decreasing) %in% c(1,length(key(x))))stop(
		"decreasing must be atomic or length(key(x))"
	)
	decreasing <- as.logical(decreasing)
	if(length(decreasing)==1)decreasing <- rep(decreasing,length(key(x)))
	names(decreasing) <- key(x)
	KEY <- data.frame(lapply(x[,key(x),drop=FALSE],function(x)as.numeric(factor(x))))
	for(col in names(decreasing))if(decreasing[[col]])KEY[[col]] <- KEY[[col]]*-1
	#for(k in rev(key(x)))x <- x[order(x[[k]],decreasing=decreasing[k]),]
	x[do.call(order,KEY),]
}

`[.keyed` <- function(x,...){
	y <- NextMethod()
	key(y) <- key(x)
	y
}

transform.keyed <- function(`_data`,...){
	x <- NextMethod('transform',`_data`,...)
	class(x) <- class(`_data`)
	key(x) <- key(`_data`)
	x
}


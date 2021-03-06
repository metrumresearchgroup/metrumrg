`as.keyed` <- function(x,...)UseMethod("as.keyed")

`as.keyed.data.frame` <- function(
	x,
	key=character(0),
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
unsorted <- function(x,...)UseMethod('unsorted')
unsorted.keyed <- function(x, decreasing=FALSE, ...)rownames(x) != rownames(sort(x,decreasing=decreasing,...))

`print.keyed.summary` <- function(x,...){
	writeLines(paste(x$key,collapse='~'))
	writeLines(paste(x$naKeys,'NA keys'))
	writeLines(paste(x$dupKeys,'duplicate keys'))
	if(x$unsorted)writeLines(paste('unsorted',x$unsorted,sep=': '))
	if(length(setdiff(names(x),c('key','naKeys','dupKeys','unsorted'))))writeLines('other attributes present')
}
	
`summary.keyed` <- function(object,...){
	x<-object
	z <- list()
	z$key <- key(x)
	z$naKeys <- sum(naKeys(x))
	z$dupKeys <- sum(dupKeys(x))
	z$unsorted <- sum(unsorted(x))
	class(z) <- 'keyed.summary'
	z	
}

`uniKey` <- function(x,...)UseMethod("uniKey")

`uniKey.keyed` <- function(x,key=NULL,...){
	if(is.null(key))key <- key(x)
	key <- x[,key,drop=FALSE]
	res <- do.call(paste,c(key,list(sep='\r')))
	class(res) <- c('uniKey',class(res))
	res
}
`c.uniKey` <- function(...,recursive=FALSE)structure(c(unlist(lapply(list(...), unclass))), class="uniKey")

`[.uniKey` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

`[[.uniKey` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

`rep.uniKey` <- function(x,...){
	y <- NextMethod()
	class(y) <- class(x)
	y
}
`format.uniKey` <- function(x,...)as.character(x,...)
as.character.uniKey <- function(x,...)unclass(gsub('\r',' ',x))

`print.uniKey` <- function(x,...){
	print(format(x),...,quote=FALSE)
	invisible(x)
}
unique.uniKey <-
function (x, incomparables = FALSE,...){
    oldclass <- class(x)
    structure(unique(unclass(x)), class = oldclass)
}
xtfrm.uniKey <- function(x)as.character(x)
as.vector.uniKey <- function(x,...)x
#unit test

#x <- data.frame(
#  x=c('a ','a'),
#  y=c('b',' b')
#)
#x
#x <- as.keyed(x, c('x','y'))
#uniKey(x)
#factor(uniKey(x))
#uniKey(x,sep='.')

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
	frozen <- cast(molten,formula=...~aggregate.keyed.variable,fun.aggregate=FUN,...)
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
	x[do.call(order,KEY),,drop=FALSE]
}

`[.keyed` <- function (
  x, 
  i,
  j, 
  drop
){
  if(!missing(j))if(inherits(j, 'character')){
    detect <- grepl('^\\*',j)
    j <- sub('^\\*','',j)
    token <- getOption('defined')
    if(is.null(token)) token <- 'defined'
    for(col in j[detect]) x[[col]] <- ifelse(is.defined(x[[col]]),token,x[[col]])    
  }
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
as.data.frame.keyed <- function(x,...){
  x <- as.data.frame.data.frame(x,...)
  attr(x,'key') <- NULL
  x
}


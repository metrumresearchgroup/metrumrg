as.digest.data.frame <- function(
  x,
  key=character(0),
  strict=TRUE,
  ...
){
  y <- .digest(x,key=character(0),other=key,strict=TRUE,descend=1,...)
  names(y) <- sapply(y,function(df)paste(key(df), collapse='.'))
  names(y)[names(y)==''] <- '.'
  if(any(dupKeys(y[[length(y)]])))names(y)[[length(y)]] <- '..'
  class(y) <- 'digest'
  y
}

.digest <- function(x,key=character(0),other=character(0),strict=TRUE,descend=1,...){
  stopifnot(descend %in% c(1,-1))
  y <- lyse(x, on=key, strict=strict,...)
  left  <- y$static
  right <- y$dynamic
  other <- other %-% left
  if(descend < 0)right <- x[,names(right) %-% (names(left) %-% key),drop=FALSE]
  left  <- .groom(left,  key=key, strict=strict,descend = -1,other=character(0),...)
  right <- .groom(right, key=key, strict=strict,descend = as.integer(as.logical(descend + 1)),other=other,...)
  c(left,right)
 }
	 
.groom <- function(x, key, other, strict, descend=0,...){
  stopifnot(descend %in% -1:1)
  if(all(names(x) %in% key)) return(NULL)
  up <- descend > 0 & length(other)
  down <- descend < 0 & length(key > 1)
  proceed <- up | down
  if(down) key <- key[-1]
  if(up)   key <- append(key, other[[1]])
  if(up)   other <- other[-1]
  if(up | down)return(.digest(x, key = key, other = other, strict = strict, descend = descend,...))
  return(list(x))
}
	 
head.digest <- function(x,...)lapply(x,head)
 	 
 	 
 	 
 	 
 	 
 	 

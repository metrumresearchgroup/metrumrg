`lyse` <- function(x,...)UseMethod('lyse')
`lyse.data.frame` <- function(x,on=character(0),strict=TRUE,...){
  stopifnot(
  	is.character(on),
  	length(on)==0 | all(on %in% names(x)),
  	is.logical(strict)
  )
  key <- rep(TRUE,length.out=nrow(x))
  if(length(on))key <- factor(do.call(paste,x[,on,drop=FALSE]))
  if(!strict)x[] <- lapply(x,function(col)reapply(col,INDEX=key,FUN=forbak))
  keep <- sapply(x,constant,within=key)
  toss <- !keep
  toss[names(x) %in% on] <- TRUE 
  kept <- x[,keep,drop=FALSE]
  kept <- unique(kept)
  kept <- as.keyed(kept, key=on)
  #class(kept) <- c('static',class(kept))
  tossed <- x[,toss,drop=FALSE]
  tossed <- as.keyed(tossed, key = on)
  #class(tossed) <- c('dynamic',class(tossed))
  return(list(static=kept,dynamic=tossed))
}
`lyse.keyed` <- function(x,on=key(x),...)lyse.data.frame(x,on=on,...)
`static` <- function(x,...)UseMethod('static')
`static.data.frame` <- function(x,on=character(0),strict=TRUE,...)lyse(x,on=on,strict=strict,...)$static
`static.keyed` <- function(x,on=key(x),...)static.data.frame(x,on=on,...)
`dynamic` <- function(x,...)UseMethod('dynamic')
`dynamic.data.frame` <- function(x,on=character(0),strict=TRUE,...)lyse(x,on=on,strict=strict,...)$dynamic
`dynamic.keyed` <- function(x,on=key(x),...)dynamic.data.frame(x,on=on,...)


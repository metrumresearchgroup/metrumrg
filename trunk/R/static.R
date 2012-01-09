`static` <- function(x,...)UseMethod('static')

`static.data.frame` <- function(x,on=character(0), ...){
  stopifnot(is.character(on))
  if(!length(on))return(unique(x))
  stopifnot(all(on %in% names(x)))
  prime <- names(x) %in% on
  key <- factor(do.call(paste,x[,on,drop=FALSE]))
  stat <- rep(FALSE,length.out=ncol(x))
  stat[prime] <- TRUE
  stat[!prime] <- sapply(x[,!prime,drop=FALSE],constant,within=key)
  unique(x[stat])
}

`static.keyed` <- function(x,on=key(x),...)static.data.frame(x,on=on,...)


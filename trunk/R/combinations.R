combinations <- function(x,...)UseMethod('combinations')
combinations.data.frame <- function(
	x,
	key=names(x),
	depth=length(key),
	detect=character(0),
	count=FALSE,
	...
){
  stopifnot(length(count)<=1)
  if(is.logical(count))if(count) count <- 'count' else count <- character(0)
  detect <- detect %n% names(x)
  token <- getOption('defined')
  if(is.null(token))token <- 'defined'
  for(col in detect) x[[col]] <- ifelse(is.na(x[[col]]),x[[col]], token)
  x <- sort(as.keyed(x, key=key[seq_len(depth)]))
  if(length(count))x[[count]] <- reapply(rownames(x), uniKey(x), length)
  x <- unique(x)
  x <- shuffle(x, key)
  x  
}


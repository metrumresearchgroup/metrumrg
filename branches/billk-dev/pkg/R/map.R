map <-
function(x,from,to,strict=TRUE,...){
	stopifnot(length(to)==length(from))
	res <- to[match(x,table=from)]
	if(!strict) res[!(x %in% from)] <- x[!(x %in% from)]
	res
}


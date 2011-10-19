map <-
function(x,from,to){
	stopifnot(length(to)==length(from))
	to[match(x,table=from)]
}


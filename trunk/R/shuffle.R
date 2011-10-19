shuffle <- function(x,who,after=NA){
	nms <- names(x)[!names(x) %in% who]
	nms <- append(nms, who, after=match(after,nms,nomatch=0))
	x[nms]
}

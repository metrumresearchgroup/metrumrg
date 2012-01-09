`contains` <-
function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
}
`%contains%` <- function(x,y)contains(y,x)
`text2decimal` <-
function (x) as.numeric(sub("^[^0-9.+-]*([0-9.+-eE]+).*$", "\\1", as.character(x)))

aug <- function(x,...){
	extras <- list(...)
	nms <- names(extras)
	for(name in nms)x[[name]] <- extras[[name]]
	x
}


`contains` <-
function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
}
`%contains%` <- function(x,y)contains(y,x)
`text2decimal` <-
function (x) as.numeric(sub("^[^0-9.+-]*([0-9.eE+-]+).*$", "\\1", as.character(x)))

aug <- function(`_data`,...){
	extras <-  eval(substitute(list(...)), `_data`, parent.frame())
	nms <- names(extras)
	for(name in nms)`_data`[[name]] <- extras[[name]]
	`_data`
}
is.defined <- function(x)!is.na(x)
pool <- function(x,y)list(x=setdiff(x,y),y=setdiff(y,x),both=intersect(x,y))
ncomma <- function(x,...)paste(names(x,...),collapse=', ')
#looks.numeric <- function(x){
#	x <- paste(x)
#	x <- gsub(' ','',x)
#	pattern <- '^[+-]?[0-9]*[.]*[0-9]*$'
#	pattern <- sub('\\.',getOption('OutDec'),pattern)
#	all(grepl(pattern=pattern,x=x))
#}
#experimental; documentation commented in file ncomma.Rd

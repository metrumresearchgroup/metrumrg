parameter2wiki <- function(x,...){
	x <- as.character(x)
	x <- titleCase(x)
	x[is.fixed(x)] <- tolower(x[is.fixed(x)])
	x <- sub('([0-9.]+)','_\\1 ',x,...)
	x
}


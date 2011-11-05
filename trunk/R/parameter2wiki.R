parameter2wiki <- function(x,lowercase='is.fixed',...){
	x <- as.character(x)
	x <- titleCase(x)
	for(f in lowercase){
		fun <- match.fun(f)
		x[fun(x)] <- tolower(x[fun(x)])
	}
	x <- sub('([0-9.]+)','_\\1 ',x,...)
	x
}


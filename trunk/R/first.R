#nth returns the nth element in x where 'where' is TRUE, 'within' each level,
#repeated for each element sharing the level. Negative values count from the end of the vector.
nth <- function(
	x,
	where,
	within,
	n=1,
	...
){
	#initialize scale
	scale <- 0
	n <- as.integer(n)
	stopifnot(length(n)==1)

	#groom supplied arguments, updating scale
	if(!missing(x)) scale <- max(scale,length(x))
	if(!missing(where)){
		where <- as.logical(where)
		scale <- max(scale,length(where))
	}
	if(!missing(within)){
		if (is.list(within))within <- as.numeric(factor(do.call(paste,within)))
		scale <- max(scale,length(within))
	}

	#compare length non-missing args to scale
	if(!missing(x))if(length(x))if(scale%%length(x)!=0)warning('scale not a multiple of x')
	if(!missing(where))if(length(where))if(scale%%length(where)!=0)warning('scale not a multiple of where')
	if(!missing(within))if(length(within))if(scale%%length(within)!=0)warning('scale not a multiple of within')

	#handle missingness with ideal defaults, conditional on best value for scale
	if(missing(x))x <- seq(length.out=scale)
	if(missing(where))where <- rep(TRUE,scale)
	if(missing(within))within <- rep(TRUE,scale)

	#in the case of supplied args, stretch to scale
	x <- rep(x,length.out=scale)
	where <- rep(where,length.out=scale)
	within <- rep(within,length.out=scale)
	
	#subset x by comparing actual where to idealized where, conditional on within
	actual <- paste(within,where)
	ideal <- paste(within,rep(TRUE,scale))
	
	#recursive, conditional on n
	if(is.na(n))return(x)
	if(n == 1)return(x[match(ideal,actual)])
	if(n == 0)return(rep(NA,scale))
	if(n <  0)return( #invert all args, process as positive, and invert the final result
		rev(
			nth(
				x=rev(x),
				where=rev(where),
				within=rev(within),
				n=-n
			)
		)
	)
	#most general case: n > 1
	#strategy: disqualify first set of values and search again with decremented n
	#nth called without x gives element indices: reduced to a unique set
	where[
		unique(
			nth(
				where=where,
				within=within,
				n=1
			)
		)
	] <- FALSE
	nth(
		x=x,
		where=where,
		within=within,
		n=n-1
	)
}
first <- function(x,where,within,...)nth(x=x,where=where,within=within,n=1)
last <-  function(x,where,within,...)nth(x=x,where=where,within=within,n=-1)
only <- function(x,where,within,...)ifelse(
	is.na(
		nth(
			x=x,
			where=where,
			within=within,
			n=2
		)
	),
	nth(
		x=x,
		where=where,
		within=within,
		n=1
	),
	NA
)
	

#logicals
distance <- function(where,within,n=1,...)nth(where=where,within=within,n=NA)-nth(where=where,within=within,n=n)
before   <- function(where,within,n=1,...)distance(where=where,within=within,n=n) < 0
at       <- function(where,within,n=1,...)distance(where=where,within=within,n=n) == 0
after    <- function(where,within,n=1,...)distance(where=where,within=within,n=n) > 0


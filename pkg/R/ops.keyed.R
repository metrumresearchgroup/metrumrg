#Operating principles
#All data.frames participating in merge syntax must inherit keyed.
#No specific Ops methods may be defined for any inherited class.
#Ops.keyed will always match both args, and redirect to method set.
#Redirect dispatches on right argument.
#Specific methods may be defined for redirect values. 

Ops.keyed <- function(e1,e2){
	methods  <- c('plus','minus','and','left')
	generics <- c('+',   '-',    '&',   '|' )
	method   <- methods[match(.Generic,generics)]
	UseMethod(method,e2)
}

`plus.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	writeLines(
		paste(
			'outer join of',
			xr,
			'rows and',
			yr,
			'rows on',
			paste(
				key,
				collapse=', '
			)
		)
	)
	merge(x,y,all=TRUE)
}

`and.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	matching <- sum(uniKey(x,key) %in% uniKey(y,key))
	writeLines(
		paste(
			'inner join of',
			xr,
			'rows and',
			yr,
			'rows on',
			paste(
				key,
				collapse=', '
			),
			'with',
			matching,
			'matches'
		)
	)
	merge(x,y)
}

`left.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	writeLines(
		paste(
			'left join of',
			xr,
			'rows and',
			yr,
			'rows on',
			paste(
				key,
				collapse=', '
			)
		)
	)	
	left <- unique(uniKey(x,key))
	right <- unique(uniKey(y,key))
	unmatched <- setdiff(left,right)
	unused <- setdiff(right,left)
	if(length(unmatched))warning(paste(length(unmatched),'unmatched keys e.g.:',unmatched[1]))
	if(length(unused))warning(paste(length(unused),'unused keys, e.g.:',unused[1]))
	merge(x,y,all.x=TRUE)
}

`minus.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	bad <- uniKey(x,key) %in% uniKey(y,key)
	writeLines(
		paste(
			'dropping',
			sum(bad),
			'of',
			xr,
			'rows matching on',
			paste(
				key,
				collapse=', '
			)
		)
	)	
	x[!bad,]
}




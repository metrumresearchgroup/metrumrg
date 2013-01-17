#Operating principles
#All data.frames participating in merge syntax must inherit keyed.
#No specific Ops methods may be defined for any inherited class.
#Ops.keyed will always match both args, and redirect to method set.
#Redirect dispatches on right argument.
#Specific methods may be defined for redirect values. 

Ops.keyed <- function(e1,e2){
	methods  <- c('plus','minus','and','left', 'times','divide','raised','mod','div','not')
	generics <- c('+',   '-',    '&',   '|',   '*',    '/',     '^',     '%%', '%/%','!' )
	method   <- methods[match(.Generic,generics)]
	if(method=='not')UseMethod(method,e1)
	UseMethod(method,e2)
}
.reportCols <- function(key){
	lim <- 7
	if(length(key) > lim) lim <- 5
	if(length(key) == 0) return('no matching columns')
	if(length(key) <= lim) return(paste(key,collapse=', '))
	#length(key) is greater than lim
	other <- length(key) - lim
	key <- key[1:lim]
	msg <- paste(key,collapse=', ')
	msg2 <- paste('and',other,'others')
	return(paste(msg,msg2))
}
	
`plus.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	matching <- sum(uniKey(x,key) %in% uniKey(y,key))
	matchmsg <- paste(' with',matching,'matches')
	message('full join of ',xr,' rows and ',yr,' rows on ', .reportCols(key), matchmsg)
	merge(x,y,all=TRUE)

}

`and.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	matching <- sum(uniKey(x,key) %in% uniKey(y,key))
	message('inner join of ',xr,' rows and ',yr,' rows on ', .reportCols(key),' with ',matching,' matches')
	merge(x,y)
}

`left.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	message('left join of ',xr,' rows and ',yr,' rows on ', .reportCols(key))	
	left <- unique(uniKey(x,key))
	right <- unique(uniKey(y,key))
	unmatched <- setdiff(left,right)
	unused <- setdiff(right,left)
	if(length(unmatched))warning(length(unmatched),' unmatched keys e.g.: ',unmatched[1])
	if(length(unused))warning(length(unused),' unused keys, e.g.: ',unused[1])
	right <- uniKey(y,key)
	dups <- duplicated(right)
	if(any(dups))warning(sum(dups),' duplicated keys, e.g.: ',right[dups][1])
	merge(x,y,all.x=TRUE)
}

`minus.keyed` <- function(x,y){
	xr <- nrow(x)
	yr <- nrow(y)
	key <- intersect(names(x),names(y))
	bad <- uniKey(x,key) %in% uniKey(y,key)
	message('dropping ',sum(bad),' of ',xr,' rows matching on ', .reportCols(key))	
	x[!bad,,drop=FALSE]
}

`divide.keyed` <- function (x, y){
  xr <- nrow(x)
  yr <- nrow(y)
  key <- intersect(names(x), names(y))
  good <- uniKey(x, key) %in% uniKey(y, key)
  message("keeping ", sum(good), " of ", xr, " rows matching on ", .reportCols(key))
  x[good, ,drop=FALSE]
}

`raised.keyed` <- function (x, y) {
  key <- key(y)
  message("serial left join of ", nrow(x), " rows and ", nrow(y), " rows on ", .reportCols(key))
  known <- names(x)[!names(x) %in% key]
  series <- lapply(seq_along(key), function(n) key[seq_len(n)])
  for(i in seq_along(series)){
    key <- series[[i]]
    # This is by definition a fishing expedition, so don't look in y cols already defined in x
    y <- y[,setdiff(names(y),known),drop=FALSE]
    z <- static(y,on=key)
    new <- setdiff(names(z),key)
    known <- union(known,new)
    try(x <- stableMerge(x, z), silent = TRUE)
  }
  x
}

`times.keyed` <- function (x, y) {
  key <- intersect(names(x), names(y))
  existing <- uniKey(y, key) %in% uniKey(x, key)
  y <- unique(y[!existing,,drop=FALSE])
  xr <- nrow(x)
  yr <- nrow(y)
  message("column-stable full join of ", xr, " existing rows and ", yr, " novel rows on ", .reportCols(key))
  merge(x, y, all = TRUE)[,names(x),drop=FALSE]
}

`not.keyed` <- function(x)x[dupKeys(x) | naKeys(x),,drop=FALSE]

`as.vector.keyed` <- function(x,mode='any')names(x)

`%+%` <- function(x,y)UseMethod('%+%')
`%&%` <- function(x,y)UseMethod('%&%')
`%u%` <- function(x,y)UseMethod('%+%')
`%n%` <- function(x,y)UseMethod('%&%')
`%-%` <- function(x,y)UseMethod('%-%')
`%+%.default` <- function(x,y)union(x,y)
`%&%.default` <- function(x,y)intersect(x,y)
`%-%.default` <- function(x,y)setdiff(x,y)



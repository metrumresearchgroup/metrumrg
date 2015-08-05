#Operating principles
#All data.frames participating in merge syntax must inherit keyed.
#No specific Ops methods may be defined for any inherited class.
#Ops.keyed will always match both args, and redirect to method set.
#Redirect dispatches on right argument.  Still current?
#Specific methods may be defined for redirect values. 

#Ops.keyed <- function(e1,e2){
#	methods  <- c('plus','minus','and','left', 'times','divide','raised','mod','div','not')
#	generics <- c('+',   '-',    '&',   '|',   '*',    '/',     '^',     '%%', '%/%','!' )
#	method   <- methods[match(.Generic,generics)]
#	if(method=='not')UseMethod(method,e1)
#	UseMethod(method,e2)
#}
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
	
`+.keyed` <- function(e1,e2){
	xr <- nrow(e1)
	yr <- nrow(e2)
	key <- intersect(names(e1),names(e2))
	matching <- sum(uniKey(e1,key) %in% uniKey(e2,key))
	matchmsg <- paste(' with',matching,'matches')
	message('full join of ',xr,' rows and ',yr,' rows on ', .reportCols(key), matchmsg)
	merge(e1,e2,all=TRUE)

}

`&.keyed` <- function(e1,e2){
	xr <- nrow(e1)
	yr <- nrow(e2)
	key <- intersect(names(e1),names(e2))
	matching <- sum(uniKey(e1,key) %in% uniKey(e2,key))
	message('inner join of ',xr,' rows and ',yr,' rows on ', .reportCols(key),' with ',matching,' matches')
	merge(e1,e2)
}

`|.keyed` <- function(e1,e2){
	xr <- nrow(e1)
	yr <- nrow(e2)
	key <- intersect(names(e1),names(e2))
	message('left join of ',xr,' rows and ',yr,' rows on ', .reportCols(key))	
	left <- unique(uniKey(e1,key))
	right <- unique(uniKey(e2,key))
	unmatched <- setdiff(left,right)
	unused <- setdiff(right,left)
	if(length(unmatched))warning(length(unmatched),' unmatched keys e.g.: ',unmatched[1])
	if(length(unused))warning(length(unused),' unused keys, e.g.: ',unused[1])
	right <- uniKey(e2,key)
	dups <- duplicated(right)
	if(any(dups))warning(sum(dups),' duplicated keys, e.g.: ',right[dups][1])
	merge(e1,e2,all.x=TRUE)
}

`-.keyed` <- function(e1,e2){
	xr <- nrow(e1)
	yr <- nrow(e2)
	key <- intersect(names(e1),names(e2))
	bad <- uniKey(e1,key) %in% uniKey(e2,key)
	message('dropping ',sum(bad),' of ',xr,' rows matching on ', .reportCols(key))	
	e1[!bad,,drop=FALSE]
}

`/.keyed` <- function (e1,e2){
  xr <- nrow(e1)
  yr <- nrow(e2)
  key <- intersect(names(e1), names(e2))
  good <- uniKey(e1, key) %in% uniKey(e2, key)
  message("keeping ", sum(good), " of ", xr, " rows matching on ", .reportCols(key))
  e1[good, ,drop=FALSE]
}

`^.keyed` <- function (e1,e2) {
  key <- key(e2)
  message("serial left join of ", nrow(e1), " rows and ", nrow(e2), " rows on ", .reportCols(key))
  known <- names(e1)[!names(e1) %in% key]
  series <- lapply(seq_along(key), function(n) key[seq_len(n)])
  for(i in seq_along(series)){
    key <- series[[i]]
    # This is by definition a fishing expedition, so don't look in y cols already defined in x
    e2 <- e2[,setdiff(names(e2),known),drop=FALSE]
    z <- static(e2,on=key)
    new <- setdiff(names(z),key)
    known <- union(known,new)
    try(e1 <- stableMerge(e1, z), silent = TRUE)
  }
  e1
}

`*.keyed` <- function (e1,e2) {
  key <- intersect(names(e1), names(e2))
  existing <- uniKey(e2, key) %in% uniKey(e1, key)
  e2 <- unique(e2[!existing,,drop=FALSE])
  xr <- nrow(e1)
  yr <- nrow(e2)
  message("column-stable full join of ", xr, " existing rows and ", yr, " novel rows on ", .reportCols(key))
  merge(e1, e2, all = TRUE)[,names(e1),drop=FALSE]
}

`!.keyed` <- function(e1)e1[dupKeys(e1) | naKeys(e1),,drop=FALSE]

`as.vector.keyed` <- function(e1,mode='any')names(e1)

`%+%` <- function(e1,e2)UseMethod('%+%')
`%&%` <- function(e1,e2)UseMethod('%&%')
`%u%` <- function(e1,e2)UseMethod('%+%')
`%n%` <- function(e1,e2)UseMethod('%&%')
`%-%` <- function(e1,e2)UseMethod('%-%')
`%+%.default` <- function(e1,e2)union(e1,e2)
`%&%.default` <- function(e1,e2)intersect(e1,e2)
`%-%.default` <- function(e1,e2)setdiff(e1,e2)



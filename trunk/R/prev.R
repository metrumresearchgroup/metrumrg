
`prev` <-
#function(x)c(NA,x[-length(x)])#last observation
function(x){
	s <- seq_along(x)
	s <- c(length(s),s[-length(s)])
	x <- x[s]
	if(length(x))x[[1]] <- NA
	x
}

`nxt` <- function(x)rev(prev(rev(x)))


`runhead` <-
function(x){#not like last observation
	n <- x != prev(x)
	if(length(n)) n[[1]] <- TRUE
	n
}

`maxChar` <-
function(x){
	x <- as.character(x)
	len <- nchar(x)
	max(len)
}


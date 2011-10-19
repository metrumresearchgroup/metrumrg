`prev` <-
function(x)c(NA,x[-length(x)])#last observation
`nxt` <- function(x)rev(prev(rev(x)))
`runhead` <-
function(x){#not like last observation
	n <- x!=prev(x)
	n[[1]] <- TRUE
	n
}
`maxChar` <-
function(x){
	x <- as.character(x)
	len <- nchar(x)
	max(len)
}


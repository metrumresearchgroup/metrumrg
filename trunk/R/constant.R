`crosses` <- function(a,b)duplicated(
	cbind(
		as.data.frame(a),
		as.data.frame(b)
	)
)!=
duplicated(
	as.data.frame(a)
)
`%crosses%` <- function(a,b)any(crosses(a,b))
`%crossed.on%` <- function(a,b)b %crosses% a
`%nests%` <- function(a,b)!b %crosses% a
`%nested.in%` <- function(a,b) b %nests% a
constant <- function(x,...)UseMethod('constant')
constant.default <- function(x,within,...)x%nests%within



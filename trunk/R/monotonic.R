nondecreasing <- function(x, ...)UseMethod('nondecreasing')
nondecreasing.default <- function(x, strict=FALSE, ...){
  if(strict) y <- x > prev(x)
  else y <- x >= prev(x)
  if(length(y)) y[[1]] <- TRUE
  y
}
nonincreasing <- function(x,...)UseMethod('nonincreasing')
nonincreasing <- function(x, strict=FALSE, ...){
  if(strict) y <- x < prev(x)
  else y <- x <= prev(x)
  if(length(y)) y[[1]] <- TRUE
  y
}
monotonic <- function(x, ...)UseMethod('monotonic')
monotonic.default <- function(x, strict=FALSE, ...)all(
	nondecreasing(x,strict=strict,...)) || 
	all(nonincreasing(x,strict=strict,...)
)


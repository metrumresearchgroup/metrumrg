
`as.vector.keyed` <- function(x,mode='any')names(x)

`%+%` <- function(x,y)UseMethod('%+%')
`%&%` <- function(x,y)UseMethod('%&%')
`%u%` <- function(x,y)UseMethod('%+%')
`%n%` <- function(x,y)UseMethod('%&%')
`%-%` <- function(x,y)UseMethod('%-%')
`%+%.default` <- function(x,y)union(x,y)
`%&%.default` <- function(x,y)intersect(x,y)
`%-%.default` <- function(x,y)setdiff(x,y)



`as.filename` <-
function(x,...)UseMethod("as.filename")
`as.filename.character` <-
function(x,...){
class(x) <- c("filename",class(x))
attr(x,'extras') <- list(...)
return(x)
}
`as.csv.filename` <-
function(x,...)UseMethod("as.csv.filename")
`as.csv.filename.character` <-
function(x,...){
class(x) <- c("csv.filename",class(x))
return(x)
}

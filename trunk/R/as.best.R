as.best <-
function(x,...)UseMethod('as.best')
as.best.data.frame <-
function(x,...){
  for(col in names(x)){
    tryCatch(
      x[[col]] <- as.best(x[[col]]), 
      error = function(e) stop(e$message)
    )
  }
  x
}
as.best.default <-
function(x,...){
  x <- as.character(x)
  x <- sub('^ *','',x)
  x <- sub(' *$','',x)
  x[x=='NA'] <- NA
  x[x==''] <- NA
  x[x=='.'] <- NA
  y <- suppressWarnings(as.numeric(x))
  newNA <- !is.na(x) & is.na(y)
  if(all(is.na(y)))return(x)
  if(any(newNA))stop((x[newNA][[1]]))
  return(y)
}

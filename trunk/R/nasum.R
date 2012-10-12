nasum <- function(x, simplify=TRUE){
  if(!is.list(x)) x <- list(x)
  if(simplify) y <- sapply(x,function(y)sum(is.na(y)))
  else y <- lapply(x,function(y)sum(is.na(y)))
  y
}


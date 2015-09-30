shuffle <- function(x,who,after=NA){
  names(x) <- make.unique(names(x))
  who <- names(x[,who,drop=FALSE]) 
  nms <- names(x)[!names(x) %in% who]
  if(is.null(after)) after <- length(nms)
  if(is.na(after)) after <- 0
  if(length(after)==0) after <- length(nms)
  if(is.character(after)) after <- match(after, nms, nomatch = 0)
  if(after < 0) after <- length(nms)
  if(after > length(nms)) after <- length(nms)
  nms <- append(nms, who, after = after)
  x[nms]
}

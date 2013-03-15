`+.timeline` <- function(e1,e2){
  if(missing(e2))return(e1)
  if(inherits(e1,'timepoint') & inherits(e2,'timepoint'))stop('addition is undefined for two timepoints')
  as <- class(e1)[[1]]
  if(inherits(e2,'timepoint')) as <- class(e2)[[1]]
  coerce <- match.fun(glue('as.',as))
  if(inherits(e1,'duration') & inherits(e2,'duration') & !identical(class(e1)[[1]],class(e2)[[1]]))message('coercing to class ',class(e1)[[1]])
  e1 <- as.numeric(as.second(e1))
  e2 <- as.numeric(as.second(e2))
  res <- as.second(e1 + e2)
  res <- coerce(res)
  res
}
	
`-.timeline` <- function(e1,e2){
  if(missing(e2)) if(any(c('mDateTime','mDate') %in% class(e1)))warning('negative mDate and mDateTime may not be meaningful')
  if(missing(e2))return(e1 * (-1))	  
  if(inherits(e2,'timepoint'))stop('subtracting a timepoint is undefined')
  as <- class(e1)[[1]]
  coerce <- match.fun(glue('as.',as))
  if(inherits(e1,'timepoint')) e2 <- as.second(e2)
  else e2 <- coerce(e2)
  e1 <- as.numeric(e1)
  e2 <- as.numeric(e2)
  res <- e1 - e2
  res <- coerce(res)
  res
}


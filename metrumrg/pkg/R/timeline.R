`+.timeline` <- function(e1,e2){
  if(missing(e2))return(e1)
  # two timepoints
  if(inherits(e1,'timepoint') && inherits(e2,'timepoint'))stop('addition is undefined for two timepoints')
  # one timepoint
  if(inherits(e1,'timepoint')){
  	as <- class(e1)[[1]]
  	e2 <- as.second(e2)
  }
  if(inherits(e2,'timepoint')){
  	as <- class(e2)[[1]]
  	e1 <- as.second(e1)
  }
  # zero timepoints, two durations
  if(inherits(e1,'duration') && inherits(e2,'duration')){
  	  as <- class(e1)[[1]]
  	  if(!identical(class(e1)[[1]],class(e2)[[1]])){
  	  	  message('coercing to ',class(e1)[[1]])
  	  	  e2 <- match.fun(glue('as.',as))(e2)
  	  }
  }  	  
  # zero timepoints, one duration
  if(inherits(e1,'duration') && !inherits(e2,'timeline')){
  	  as <- class(e1)[[1]]
  	  e2 <- match.fun(glue('as.',as))(e2)
  }
  if(inherits(e2,'duration') && !inherits(e1,'timeline')){
  	  as <- class(e2)[[1]]
  	  e1 <- match.fun(glue('as.',as))(e1)
  }
  e1 <- as.numeric(e1)
  e2 <- as.numeric(e2)
  res <- e1 + e2
  res <- match.fun(glue('as.',as))(res)
  res
}
	
`-.timeline` <- function(e1,e2){
  if(missing(e2)) if(any(c('mDateTime','mDate') %in% class(e1)))warning('negative mDate and mDateTime may not be meaningful')
  if(missing(e2))return(e1 * (-1))
  # x - timepoint
  #if(inherits(e2,'timepoint'))stop('subtracting a timepoint is undefined')
  if(inherits(e2,'timepoint') && !inherits(e1,'timepoint') )stop('subtracting a timepoint from non-timepoint is undefined')
  # timepoint - timepoint
  # timepoint - duration
  # timepoint - any
  if(inherits(e1,'timepoint')){
  	  if(inherits(e2,'timepoint'))e1 <- as.second(e1)
  	  as <- class(e1)[[1]]
  	  e2 <- as.second(e2)
  }
  # duration - duration
  # duration - any
  if(inherits(e1,'duration')){
  	  as <- class(e1)[[1]]
  	  e2 <- match.fun(glue('as.',as))(e2)
  }
  # any - duration
  if(!inherits(e1,'timeline')){ 
  	  as <- class(e2)[[1]]
  	  e1 <- match.fun(glue('as.',as))(e1)
  }
  e1 <- as.numeric(e1)
  e2 <- as.numeric(e2)
  res <- e1 - e2
  res <- match.fun(glue('as.',as))(res)
  res
}


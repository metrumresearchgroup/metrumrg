as.best <-
function(x,...)UseMethod('as.best')
as.best.data.frame <- function(x,...){
  for(col in names(x)){
    tryCatch(
      x[[col]] <- as.best(x[[col]],...), 
      error = function(e) stop('in column ',col,': ',e$message)
    )
  }
  x
}
as.best.default <-
function(x,prefix='#',na.strings=c('.','NA',''),...){
  stopifnot(length(prefix)<=1)
  x <- as.character(x)
  x <- sub('^\\s*','',x)
  x <- sub('\\s*$','',x)
  x[x %in% na.strings] <- NA
  y <- suppressWarnings(as.numeric(x))
  newNA <- !is.na(x) & is.na(y)
  if(all(is.na(y)))return(x) # nothing converted to numeric
  if(!any(newNA))return(y) # no loss on conversion to numeric
  if(!length(prefix))stop('character values mixed with numeric, e.g. ', x[newNA][[1]])
  # If we reached here, x has some values coercible to numeric and some not, maybe some NA.
  # Numeric values buried in a character vector are ambiguous
  x[!is.na(y)] <- glue(prefix,x[!is.na(y)])
  return(x)
}
as.best.comment <- function(x,...)as.numeric(as.logical(x))
as.best.temporal <- function(x,...)as.numeric(x)


supp <- function(x, suppx,...)UseMethod('supp')
supp.data.frame <- function(x, suppx, ...){
  x <- as.keyed(x)
  suppx <- as.keyed(suppx)
  supp(x, suppx, ...)
}
supp.keyed <- function(x, suppx, value = 'QVAL', ...){ # or value = QORIG or QEVAL
  names(suppx)[names(suppx) == 'RDOMAIN'] <- 'DOMAIN'
  suppx <- as.best(suppx, prefix='')
  idvar <- unique(suppx$IDVAR)
  if(any(is.na(idvar)))stop('NA IDVAR')
  if(length(idvar) == 0) return(x)
  this <- idvar[length(idvar)]
  x <- supp(x, suppx[suppx$IDVAR != this,,drop=FALSE],...)
  suppx <- suppx[suppx$IDVAR == this,,drop=FALSE]
  # now this value of IDVAR is scalar
  labels <- unique(suppx[,c('QNAM','QLABEL')])
  suppx <- cast(suppx, STUDYID + DOMAIN + USUBJID + IDVARVAL ~ QNAM, value = value,...)
  names(suppx)[names(suppx) == 'IDVARVAL'] <- this
  suppx$IDVAR <- NULL
  res <- stableMerge(x, suppx)
  for(col in labels$QNAM)attr(res[[col]],'label') <- labels$QLABEL[labels$QNAM == col]
  res
}


reapply <- 
  function (x, INDEX, FUN, where, ...) 
  {
    if(missing(where))where <- rep(TRUE,length(x))
    where <- rep(where,length.out=length(x))
    where[is.na(where)] <- TRUE
    where <- factor(where,levels=c(FALSE,TRUE))
    if(!is.list(INDEX)) INDEX <- list(INDEX)
    INDEX <- do.call(interaction,c(INDEX,list(drop=TRUE)))
    WINDEX <- interaction(where,INDEX,drop=FALSE)
    wvals <- split(x,WINDEX)
    vals <- wvals[c(FALSE,TRUE)]
    stopifnot(identical(length(vals),length(levels(INDEX))))
    val <- lapply(vals, FUN, ...)
    dim <- table(INDEX)
    val <- lapply(seq_along(val),function(i)rep(val[[i]], length.out=dim[[i]]))
    split(x,INDEX) <- val
    x[is.na(INDEX)] <- NA
    x
  }

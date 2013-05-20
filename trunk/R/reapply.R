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
    if(length(val)) if(length(val[[1]]) t <- val[[1]][[1]] # sample type given by FUN
    t <- NA # default to logical, preserve class if present
    y <- rep(t,length(x))
    split(y,INDEX) <- val # minimal bias with respect to class
    y[is.na(INDEX)] <- NA # probably true already
    y
  }

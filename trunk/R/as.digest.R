as.best.digest <- function(x,...){
  x[] <- lapply(x,as.best)
  x
}
digest <- function(x,...)UseMethod('as.digest')
as.digest <- function(x,...)UseMethod('as.digest')
as.digest.keyed <- function(
  x,
  key=match.fun('key')(x),
  strict=TRUE,
  ...
)as.digest.data.frame(x,key=key,strict=strict,...)
as.digest.nm <- function(x,key=match.fun('key')(x),...)as.digest(as.keyed(x,key=key,...))
`[.digest`<-function (x, ...) 
{
    y <- NextMethod()
    class(y) <- class(x)
    y
}
as.digest.digest <- function(x,...)x
as.keyed.nm <- function (x, key=match.fun('key')(x), ...){
    class(x) <- setdiff(class(x),'nm')
    x <- as.keyed(x,key)
    x
}
motif <- function(x,...)UseMethod('as.motif')
as.motif <- function(x,...)UseMethod('as.motif')
as.motif.keyed <- function(x,...){
  motif <- list(y='.')
  if(length(key(x))) motif$x <- key(x)[[1]]
  if(length(key(x))>1)motif$z <- key(x)[[2]]
  class(motif) <- 'motif'
  motif
}
as.motif.digest <- function(x,...){
  keys <- lapply(x,key)
  keys <- unlist(keys)
  keys <- unique(keys)
  keys <- c('.',keys)
  motif <- list(
    y=keys,
    x=keys,
    z=keys
  )
  class(motif) <- 'motif'
  motif
}
as.motif.motif <- function(x,...)x
as.motif.nm <- function(x,...){
  use <- c('TIME','ID','SEQ',key(x))
  use <- unique(use)
  use <- use[use %in% names(x)]
  motif <- list(
    y='.',
    x=use,
    z=use
  )
  class(motif) <- 'motif'
  motif
}
roles <- function(x,...)UseMethod('as.roles')
as.roles <- function(x,...)UseMethod('as.roles')
as.roles.digest <- function(x,...)lapply(x,as.roles,...)
as.roles.keyed <- function(x,motif=as.motif(x),...){
  available <- function(col,x,result){
    available <- FALSE
    if(col %in% names(x) | col=='.')
      if(!col %in% unlist(result))
        available <- TRUE
    available
  }
  res <- list()
  for(role in names(motif))
    for(opt in motif[[role]])
      if(is.null(res[[role]]))
        if(available(opt,x,res))
          res[[role]] <- opt
  res <- unlist(res)
  res
}
plot.digest <- function(
  x,
  motif=as.motif(x),
  roles=as.roles(x,motif=motif),
  ...
){
  stopifnot(length(roles)==length(x))
  plottable <- function(x)!any(is.na(key(x))) & length(key(x)) > 0
  keep <- sapply(x,plottable)
  indices <- seq_along(x)[keep]
  res <- lapply(indices,function(i,...)plot(x[[i]],roles=roles[[i]],...),...)
  res <- unlist(res,recursive=FALSE)
  res
}
index.digest <- function(
  x,
  motif=as.motif(x),
  roles=as.roles(x,motif=motif),
  ...
){
  stopifnot(length(roles)==length(x))
  plottable <- function(x)!any(is.na(key(x))) & length(key(x)) > 0
  keep <- sapply(x,plottable)
  indices <- seq_along(x)[keep]
  res <- lapply(indices,function(i,...)index(x[[i]],roles=roles[[i]],...),...)
  res <- unlist(res,recursive=FALSE)
  res
}
legacy <- function(x,...)attr(x,'legacy')
`legacy<-` <- function(x,value){
  if(is.null(names(value)))stop('value must have a name')
  attr(x,'legacy') <- c(attr(x,'legacy'),value)
  x
}

format.legacy <- function(x,...){
  f <- legacy(x)
  if(is.null(f))return('')
  vec <- sapply(names(f),function(nm)paste(nm,'=',f[[nm]]))
  vec <- paste(vec,collapse='\\n')
  vec
}
as.conditioned <- function(x,...)UseMethod('as.conditioned')
as.conditioned.isolated <- function(x,...){
  warning('isolated objects have no conditioning variables by definition')
  x
}
as.conditioned.keyed <- function(x,roles=as.roles(x),...){
  #every key that is not a role must be a classifier: recurse
  if(!all(key(x) %in% roles)){
    #identify col as the last (in canonical order) of the classifiers
    classifiers <- setdiff(key(x),roles)
    col <- classifiers[[length(classifiers)]]
    dex <- x[[col]] # save for local use
    x[[col]] <- NULL # remove from forwarded context
    key(x) <- setdiff(key(x),col) # remove from key if present
    dat <- split(x,dex,drop=TRUE) # split into sub-frames for sub-plots
    for(nm in names(dat))legacy(dat[[nm]]) <- structure(nm,names=col) #internal accumulator
    for(nm in names(dat))if(!inherits(dat[[nm]],'conditioned'))class(dat[[nm]]) <- c('conditioned',class(dat[[nm]]))
    names(dat) <- paste(col,names(dat),sep=':') # external hierarchical indicator
    res <- lapply(dat,as.conditioned,roles=roles,...)
    res <- unlist(res,recursive=FALSE)
    return(res)
  }
  if(!inherits(x,'conditioned'))class(x) <- c('conditioned',class(x))
  list(x)
} 
`[.conditioned`<-function (x, ...) 
{
    y <- NextMethod()
    if(!is.null(legacy(x)) & is.null(legacy(y))) legacy(y) <- legacy(x)
    y
}
as.conditioned.digest <- function(x,...){
  res <- lapply(x,as.conditioned,...)
  res <- unlist(res,recursive=FALSE)
  res
}
as.isolated.digest <- function(x,...){
  res <- lapply(x,as.isolated,...)
  res <- unlist(res,recursive=FALSE)
  res
}
#as.conditioned.conditioned <- function(x,...)x
as.isolated <- function(x,...)UseMethod('as.isolated')
as.isolated.isolated <- function(x,...)x
as.isolated.keyed <- function(x,...){
  targets <- setdiff(names(x),key(x))
  result <- lapply(
    targets,
    function(column){
      res <- x[,names(x) %in% c(key(x),column),drop=FALSE]
      class(res) <- c('isolated',class(res))
      res
    }
  )
  names(result) <- targets
  return(result)
}
plot.keyed <- function(x,roles=as.roles(x),...){
  tiles <- as.conditioned(x,roles=roles)
  lapply(tiles,plot,roles=roles)
}
splom.keyed <- function(x,data=NULL,roles=as.roles(x),...){
  tiles <- as.conditioned(x,roles=roles)
  lapply(tiles,splom,roles=roles)
}
plot.conditioned <- function(x,roles=as.roles(x),...){
  splom <- splom(x,roles=roles,...)
  index <- index(x,roles=roles,...)
  result <- list(splom,index)
  names(result) <- c('splom','index')
  return(result)
}
plot.isolated <- function(x,...)index(x,...)


splom.conditioned <- function(
  x,
  data=NULL,
  roles=as.roles(x),
  main='',
  xlab='',
  pscales=0,
  ...
){
  main <- format.legacy(x)
  names(x)[names(x)==roles['y']] <- 'y'
  names(x)[names(x)==roles['x']] <- 'x'
  names(x)[names(x)==roles['z']] <- 'z'
  x <- as.data.frame(x)
  x$x <- NULL
  x <- x[,sapply(x,function(col)!all(is.na(col))),drop=FALSE]
  args <- list()
  args$x <- x
  args$data=NULL
  args$main <- main
  args$xlab <- paste('for each',roles['x'])
  args$pscales <- pscales
  if('z' %in% names(x)){ # have grouping argument
    args$groups <- x$z
    args$x$z <- NULL
    args$sub <- paste('grouped by',roles['z'])
  }
  args <- c(args,list(...))
  if(ncol(args$x)<2) return(NULL)
  do.call(splom,args)
}
splom.digest <- function(
  x,
  data=NULL,
  motif=as.motif(x),
  roles=as.roles(x,motif=motif),
  main='',
  xlab='',
  pscales=0,
  ...
){
  stopifnot(length(roles)==length(x))
  plottable <- function(x)!any(is.na(key(x))) & length(key(x)) > 0
  keep <- sapply(x,plottable)
  indices <- seq_along(x)[keep]
  res <- lapply(
    indices,
    function(i,...)splom(
      x[[i]],
      roles=roles[[i]]
      ,...
    ),
    main=main,
    xlab=xlab,
    pscales=pscales,
    ...
  )
  res <- unlist(res,recursive=FALSE)
  res
}
index <- function(x,...)UseMethod('index')
index.keyed <- function(x,roles=as.roles(x),...){
  tiles <- as.conditioned(x,roles=roles)
  lapply(tiles,index,roles=roles)
}
index.nm <- function(x,density=20,...){
  if('set' %in% names(x))stop("'set' is a reserved label in index.nm")
  x$ID <- factor(as.integer(as.character(x$ID)))
  x$set <- map(x$ID,from=unique(x$ID),to=rep(seq_along(unique(x$ID)),each=density,length.out=length(unique(x$ID))))
  index(as.keyed(x,c('ID','set')),roles=c(y='.',x='ID'))
}
index.conditioned <- function(x,roles=as.roles(x),...){ 
  isolates <- as.isolated(x) # named list of isolates
  result <- lapply(isolates,index,roles=roles)
  return(result)
}
index.isolated <- function(x,roles=as.roles(x),...){
  #canonical order is the isolated column, followed by the key
  #in an isolate, we know we have only key colums and one variable.
  #at this point, we also have some assigned roles, or maybe none.
  #anything unassigned is used as a manual conditioning variable, a.o.t. lattice conditioning.
  #we can fully inform the roles at this point, because we know what '.' means
  #then we can recurse until we have nothing but roles.
  canon <-c(setdiff(names(x),key(x)),key(x))
  roles[roles=='.'] <- canon[[1]]
  names(x)[names(x)==roles['y']] <- 'y'
  names(x)[names(x)==roles['x']] <- 'x'
  names(x)[names(x)==roles['z']] <- 'z'
if(is.character(x$x))x$x <- factor(x$x)
  if(is.character(x$y))x$y <- factor(x$y)
  # now everything is presumably factor or numeric (i.e. plottable)
  args <- list()
  args$x <- y~x # first arg is formula, for dispatch on formula method
  if('z' %in% names(roles)){ # have grouping argument
    args$groups <- expression(z)
    args$sub <- paste('grouped by',roles['z'])
  }
  args$data <- x
  args$aspect <- 1
  args$xlab <- roles['x']
  args$ylab <- roles['y']
  args$main <- format.legacy(x)
  args$scales <- list()
  args$scales$tck <- c(1,0)
  args$scales$x <- list()
  if(!is.numeric(x$x))args$scales$x$rot <- 90 
  #rotate factor labels on x axis to prevent overlap
  args <- c(args,list(...))
  do.call(xyplot,args)
}

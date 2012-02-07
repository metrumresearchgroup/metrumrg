as.best.comment <- function(x,...)as.numeric(as.logical(x))
as.best.temporal <- function(x,...)as.numeric(x)
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
as.digest.data.frame <- function(
  x,
  key=character(0),
  strict=TRUE,
  mask=rep(FALSE,length(key)),
  ...
){
  debug <- FALSE
  if('debug' %in% names(list(...)))debug <- list(...)$debug
  descendable <- function(x)if(length(x)) !x[length(x)] else FALSE# terminal element is false
  descend <-function(x){ # always at least one trailing false when descending
    x[at(runhead(x),n=-1) & !x] <- TRUE
    x
  }
  ascendable <- function(x)sum(x)>1 # at least two active key members
  ascend <- function(x){
    x[match(TRUE,x)] <- FALSE # sum mask is always greater than 1 when ascending
    x
  }
  moot <- function(x,key)all(names(x)%in%key)
  if(debug)cat(paste(names(x),collapse=' '))
  if(debug)cat('\n')
  if(debug)cat('key',paste(key,collapse=' '),'\n')
  if(debug)cat('mask',paste(mask,collapse=' '),'\n')
  #if(!all(key[mask] %in% names(x)))browser()
  stopifnot(is.character(key),all(key[mask] %in% names(x)))
  if(debug)cat('lysing on',paste(key[mask],collapse=' '),' ...\n')
  lysis <- lyse(x,on=key[mask],strict=strict,...)
  stc <- lysis$static
  dyn <- lysis$dynamic
  stc <- as.keyed(stc,key[mask])
  dyn <- as.keyed(dyn,NA)
  if(debug)cat('stc: ',paste(names(stc),collapse=' '),'\n')
  if(debug)cat('dyn: ',paste(names(dyn),collapse=' '),'\n')
  if(moot(stc,key)){
    if(debug)cat('stc is moot\n') 
    stc <- list()
  }else 
  if(ascendable(mask)){
    if(debug)cat('ascending stc ...\n')
    tmp <- digest(
      stc,
      key=key,
      strict=strict,
      mask= ascend(mask),
      ...
    )
    #I had a static data frame that was not moot (had non-key columns)
    #and had at least two key columns.  I digested it.  It seems impossible
    #that digest returned nothing. Digest always returns a list of data.frames,
    #perhaps empty, but in this case has at least one data.frame, maybe more.
    #If ascension was completely unsuccessful, the static portion will have been 
    #discarded and the dynamic portion will be identical to the input.
    #If ascension was partly successful, there will be a static portion and 
    #a dynamic portion.  If ascension was completely successful, the dynamic 
    #portion will have been discarded as moot, and one or more static portions are returned.
    #Currently, the dynamic portion, if present, will be in last place.  
    #It would seem difficult to know the dynamic portion by position alone, since 
    #a single result could be a single static result, and multiple result could be
    #multiple static (?) or static/dynamic.  However, we know that only a call to digest
    #is involved, and all parsing of digested data frames happens at the single call to
    #lyse above, and that immediately thereafter both subresults are keyed diagnostically.
    #The list names are also diagnostic, but we have plans to overwrite them later.
    
    #Now, it seems tragic that a data.frame recognized as static, that is ascended without success,
    #is hobbled, i.e. static recognition on a good key is dropped in favor of dynamic 
    #recognition with respect to a partial key.  We address here.  We decide that 
    #the point of digestion is fundamentally to represent stable (static) relationships, which
    #therefore have priority.  Desecension is what it is.  Ascension, however, should be merely
    #subtractive with respect to the ascended data frame; never destructive of its key or status.
    #So, if ascension is completely successful, consuming all non-key columns and returning 
    #respective static data frames (in which case the dynamic portion will be missing)
    #we will replace the existing stc with the ascension.  If the ascension fails completely, 
    #the single returning data.frame will be a key-degraded version of the input, and we will 
    #prefer the latter.  If ascension is partly successful, we will carry the static portion of
    #the ascension, drop the key-degraded dynamic portion, and retain the static input, less any
    #reassigned columns.  Column reassignment could be recognized as non-key columns present in 
    #static (possibly multiple frames) or missing from dynamic (only one frame, we think).
    
    #The overall goal is to reassign stc as some combination of stc, ascended-static, and ascended-dynamic.
    #stc on-hand is known static on key[mask].  Effectively, ascension cannot return a dynamic data frame.
    endsDynamic <- function(x) is.na(key(x[[length(x)]]))
    ascensionSucceeded <- function(x)!endsDynamic(x)
    ascensionFailed <-    function(x)endsDynamic(x) & length(x)==1  
    ascensionPartial <-   function(x)endsDynamic(x) & length(x) > 1
    staticPart <- function(x)if(endsDynamic(x)) x[-length(x)] else x
    scavengedCols <- function(x,key)setdiff(unlist(lapply(staticPart(x),names)),key)
    reduced <- function(x,ref)x[,!names(x) %in% scavengedCols(ref,key(x)),drop=FALSE]
    if(ascensionSucceeded(tmp))stc <- tmp
    if(ascensionPartial(tmp)) stc <- c(staticPart(tmp),list(static=reduced(stc,tmp)))
    if(ascensionFailed(tmp)) stc <- list(static=stc)
  } else {
    if(debug)cat('no subkey on stc\n')
    stc <- list(static=stc)
  }
  if(moot(dyn,key)){
    if(debug)cat('dyn is moot\n')
    dyn <- list()
  }else 
  if(descendable(mask) & all(key[descend(mask)] %in% names(x))){
    if(debug)cat('descending dyn ...\n')
    dyn <- digest(
      dyn,
      key=key,
      strict=strict,
      mask=descend(mask),
      ...
    )
  }else {
    if(debug)cat('no unvisited keys\n')
    dyn <- list(dynamic=dyn)
  }
  #stc and dyn are lists, whether or not populated
  lysis <- c(stc,dyn)
  names(lysis) <- sapply(lapply(lysis,function(f)key(f)),paste,collapse=';')
  class(lysis) <- 'digest'
  lysis
}
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
    dat <- split(x,dex) # split into sub-frames for sub-plots
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
    if(!is.null(legacy(x))) legacy(y) <- legacy(x)
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

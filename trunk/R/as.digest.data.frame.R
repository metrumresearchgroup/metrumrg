as.digest.data.frame <- function(
  x,
  key=character(0),
  strict=TRUE,
  mask=rep(FALSE,length(key)),
  ...
){
  debug <- FALSE
  if('debug' %in% names(list(...)))debug <- list(...)$debug
  exhausted <- function(x)!length(x) | x[length(x)] # no key, or last(all) x are true
  ascended <- function(x)sum(x) & !x[1] # At least one true, but not the first
  descendable <- function(x)!exhausted(x) & !ascended(x)
  descend <-function(x){ # always at least one trailing false when descending
    x[at(runhead(x),n=-1) & !x] <- TRUE # the last runhead that is also false
    x
  }
  ascendable <- function(x)sum(x)>1 # at least two active key members
  ascend <- function(x){
    x[match(TRUE,x)] <- FALSE # sum mask is always greater than 1 when ascending
    x
  }
  moot <- function(x,key)all(names(x)%in%key)
  #if(debug)cat(paste(names(x),collapse=' '))
  #if(debug)cat('\n')
  #if(debug)cat('key',paste(key,collapse=' '),'\n')
  #if(debug)cat('mask',paste(mask,collapse=' '),'\n')
  #if(!all(key[mask] %in% names(x)))browser()
  stopifnot(is.character(key),all(key[mask] %in% names(x)))
  if(debug)cat('lysing on',paste(key[mask],collapse=' '),' ...\n')
  lysis <- lyse(x,on=key[mask],strict=strict,...)
  stc <- lysis$static
  dyn <- lysis$dynamic
  stc <- as.keyed(stc,key[mask])
  dyn <- as.keyed(dyn,NA)
  #dyn <- as.keyed(dyn,key %n% names(dyn))
  if(descendable(mask)){
    if(!all(key[descend(mask)] %in% names(dyn))){
        if(debug)cat('restoring keys\n')
        suppressMessages(dyn <- dyn + stc[,names(stc) %in% key,drop=FALSE])
	# restore column order
	dyn <- dyn[,intersect(names(x),names(dyn))]
      }
  }
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
    endsDynamic <- function(x) any(is.na(key(x[[length(x)]])))#scalar NA or vector key
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
  }else{ 
    if(descendable(mask)){
      if(debug)cat('descending dyn ...\n')    
      dyn <- digest(
        dyn,
        key=key,
        strict=strict,
        mask=descend(mask),
        ...
      )
    }else{
      dyn <- list(dynamic=dyn)
      if(debug){
        if(exhausted(mask)){
      	  cat('dyn key exhausted\n')
        }else{ 
      	  cat('dyn key already ascended\n')
        }
      }
    }
  }
  #stc and dyn are lists, whether or not populated
  lysis <- c(stc,dyn)
  names(lysis) <- sapply(lapply(lysis,function(f)key(f)),paste,collapse='.')
  if('NA' %in% names(lysis))lysis[['NA']] <- structure(lysis[['NA']],key=key) # restore full key for diagnostics
  class(lysis) <- 'digest'
  lysis
}
head.digest <- function(x,...)lapply(x,head)


as.digest.data.frame <- function(
  x,
  key=character(0),
  strict=TRUE,
  ...
){
  y <- .digest(x,key=character(0),other=key,strict=TRUE,descend=1,...)
  names(y) <- sapply(y,function(df)paste(key(df), collapse='.'))
  names(y)[names(y)==''] <- '.'
  if(any(dupKeys(y[[length(y)]])))names(y)[[length(y)]] <- '..'
  class(y) <- 'digest'
  y
}

.digest <- function(x,key=character(0),other=character(0),strict=TRUE,descend=1,...){
  stopifnot(descend %in% c(1,-1))
  y <- lyse(x, on=key, strict=strict,...)
  left  <- y$static
  right <- y$dynamic
  other <- other %-% left
  if(descend < 0)right <- x[,names(right) %-% (names(left) %-% key),drop=FALSE]
  left  <- .groom(left,  key=key, strict=strict,descend = -1,other=character(0),...)
  right <- .groom(right, key=key, strict=strict,descend = as.integer(as.logical(descend + 1)),other=other,...)
  c(left,right)
 }
	 
.groom <- function(x, key, other, strict, descend=0,...){
  stopifnot(descend %in% -1:1)
  if(all(names(x) %in% key)) return(NULL)
  up <- descend > 0 & length(other)
  down <- descend < 0 & length(key > 1)
  proceed <- up | down
  if(down) key <- key[-1]
  if(up)   key <- append(key, other[[1]])
  if(up)   other <- other[-1]
  if(up | down)return(.digest(x, key = key, other = other, strict = strict, descend = descend,...))
  return(list(x))
}
  
	 
 	 
 	 
 	 
 	 
 	 
 	 

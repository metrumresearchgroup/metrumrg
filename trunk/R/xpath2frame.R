.xpath2vector <- function(x, doc, fun=xmlValue,...){
  stopifnot(length(x)==1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- xmlParse(doc, asText = !file, error = NULL)
  result <- xpathSApply(doc, x, fun = fun)
  if(local)free(doc)
  result
}

xmlValue.XMLAttributeValue <- function(x,...)as.best(x)

.unstrsplit <- function(x,split,...){
  if(!is.list(x)) x <- list(x)
  split <- rep(split,length.out=length(x))
 sapply(seq_along(x),function(i)paste(x[[i]],collapse=split[[i]],...),...)
}

.xmlTerminal <- function(x, depth,...){
  x <- strsplit(x,'/')[[1]]
  x <- x[x!='']
  dimension <- length(x)
  stopifnot(depth <= dimension)
  depth == dimension  
}

.token <- function (x, depth, ...){
  x <- strsplit(x,'/')[[1]]
  x <- x[x!='']
  x[depth]
}

.xmlCount <- function(x, doc, depth, ...){
  stopifnot(length(x)==1,depth >= 1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- xmlParse(doc, asText = !file, error = NULL)
  x <- strsplit(x,'/')[[1]]
  node <- x != ''
  node <- cumsum(node)
  x <- x[node <= depth]
  x <- .unstrsplit(x,'/')
  x <- glue('count',parens(x))
  y <- .xpath2vector(x=x,doc=doc,...)
  stopifnot(length(y) == 1)
  if(local)free(doc)
  y
}

.xmlPredicate <- function(x, depth, i, ...){
  stopifnot(length(x)== 1, depth >=1)
  x <- strsplit(x,'/')[[1]]
  node <- x != '' # up to 2 leading '' are present
  node <- cumsum(node)
  index <- match(depth, node) # actual position corresponding to nth term
  pred <- function(subscript){
    y <- x
    y[index] <- glue(y[index], '[', subscript, ']')
    y
  }
  preds <- lapply(i, pred)
  preds <- sapply(preds, .unstrsplit, split='/')
  preds
}
      
.xpath2index <- function(x, doc, depth=1, ...){
  # enumerate myself, convert to df with token as col name
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- xmlParse(doc, asText = !file, error = NULL)
  dim <- .xmlCount(x = x, doc= doc, depth = depth,...)
  col <- data.frame(seq_len(dim))
  names(col) <- .token(x, depth = depth)
  # if I am terminal, return
  if(.xmlTerminal(x=x, depth=depth,...))return(col)
  # collect and enumerate children
  families <- .xmlPredicate(x, depth = depth, i = seq_len(dim), ...)
  children <- lapply(families, .xpath2index, doc = doc, depth = depth + 1, ...)
  enumeration <- sapply(children, nrow)
  # rbind children
  reunion <- do.call(rbind, children)
  # rep(myself, times=enumeration)
  col <- col[rep(rownames(col), times = enumeration),,drop=FALSE]
  # cbind(myself, children)
  stopifnot(nrow(col) == nrow(reunion))
  reunion <- cbind(col, reunion)
  # return
  if(local)free(doc)
  reunion
}

.xmlSimplify <- function(x, stack=FALSE, ignore=character(0)){
  nms <- lapply(x, unique)
  stat <- sapply(nms, function(nm)length(nm) == 1 && length(nm[is.defined(nm)]) == 1)
  stat <- names(x)[stat] %-% ignore
  x <- x[,names(x) %-% stat,drop=FALSE]
  if(!stack) return(x)
  nms <- nms[names(nms) %-% stat]
  semi <- sapply(nms, function(nm)length(nm) == 2 && length(nm[is.defined(nm)]) == 1)
  semi <- names(x)[semi] %-% ignore
  if(!length(semi))return(x)
  node <- 'node'
  if(node %in% names(x)) node <- 'metrumrg:node'
  x[node] <- NA
  x <- shuffle(x, c(node,'value'), NULL)
  for(column in semi){
    disposable <- all( is.na( x[ node ][ is.defined( x[ column ] ) ] ) )
    if(disposable) x[ node ][ is.defined( x[ column ] ) ] <- column
    if(disposable) x[column] <- NULL
  }
  x
}

.xpath2frame <- function(x, doc, fun=xmlValue, simplify = TRUE, ...){
  stopifnot(length(x)==1, length(file)==1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- xmlParse(doc, asText = !file, error = NULL)
  index <- .xpath2index(x, doc,...)
  value <- .xpath2vector(x, doc, fun=xmlValue,...)
  stopifnot(nrow(index) == length(value))
  result <- cbind(index, value, stringsAsFactors = FALSE)
  if(local)free(doc)
  if(simplify) result <- .xmlSimplify(result)
  return(result)
}

xpath2frame <- function(x, doc, simplify = TRUE, ...){
  stopifnot(length(file)==1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- xmlParse(doc, asText = !file, error = NULL)
  result <- lapply(x,.xpath2frame, doc= doc, simplify = FALSE, ...)
  if(local)free(doc)
  result <- metaMerge(result)
  result <- shuffle(result, 'value',after=NULL)
  node <- 'node'
  if(node %in% names(result)) node <- 'metrumrg:node'
  if(simplify) result <- .xmlSimplify(result, stack = TRUE)
  return(result)
}

xlog <- function(
  run, 
  project = getwd(), 
  rundir = filename(project,run), 
  file= filename(rundir,run,'.xml'), 
  xpath=c(
      '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:estimation_method',
      '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:estimation_title',
      '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:termination_status',
      '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:final_objective_function',
      '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:theta/nm:val',
      '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:omega/nm:row/nm:col',
      '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:sigma/nm:row/nm:col'
  ),
  includeFile = NULL,
  simplify = TRUE,
  ...
){
  stopifnot(xor(missing(run), missing(file)))
  mrun <- missing(run)
  if(mrun) includeFile <- TRUE
  if(is.null(includeFile)) includeFile <- FALSE
  exists <- file.exists(file)
  if(any(!exists))warning('One or more files does not exist, e.g. ', file[!exists][[1]])
  file <- file[exists]
  if(!mrun) run <- run[exists]
  result <- lapply(file, function(doc) xpath2frame(x=xpath, doc=doc, simplify = FALSE, ...))
  fnm <- 'file'
  if(fnm %in% names(result))fnm <- 'metrumrg:file'
  rnm <- 'run'
  if(rnm %in% names(result))rnm <- 'metrumrg:run'
  identify <- function(i){
    dat <- result[[i]]
    if(mrun || includeFile) dat[fnm] <- file[[i]]
    if(!mrun) dat[rnm] <- run[[i]]
    dat
  }
  result <- lapply(seq_along(result), identify)
  result <- metaMerge(result)
  result <- shuffle(result, c(rnm,fnm) %n% names(result))
  result <- shuffle(result, 'value',after=NULL)  
  node <- 'node'
  if(node %in% names(result)) node <- 'metrumrg:node'
  if(simplify) result <- .xmlSimplify(result, stack=TRUE, ignore=c(fnm,rnm))
  return(result)
}




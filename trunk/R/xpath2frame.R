.xpath2vector <- function(x, doc, fun = xmlValue, namespaces = FALSE, ...){
  stopifnot(length(x)==1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces, ...)
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

.xmlCount <- function(x, doc, depth, namespaces = FALSE, ...){
  stopifnot(length(x)==1,depth >= 1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces, ...)
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
      
.xpath2index <- function(x, doc, depth=1, namespaces = FALSE, ...){
  # enumerate myself, convert to df with token as col name
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces, ...)
  dim <- .xmlCount(x = x, doc = doc, depth = depth,...)
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
.newcolname <- function(base,existing){
	stopifnot(length(base)==1)
	nm <- base
	count <- 0
	while(nm %in% existing){
		count <- count + 1
		nm <- paste(base,count,sep='.')
	}
	return(nm)
}
		
	
.xmlSimplify <- function(x, stack=FALSE, ignore=character(0),nodebase='node',...){
  nms <- lapply(x, unique)
  stat <- sapply(nms, function(nm)length(nm) == 1 && length(nm[is.defined(nm)]) == 1)
  stat <- names(x)[stat] %-% ignore
  x <- x[,names(x) %-% stat,drop=FALSE]
  if(!stack) return(x)
  nms <- nms[names(nms) %-% stat]
  semi <- sapply(nms, function(nm)length(nm) == 2 && length(nm[is.defined(nm)]) == 1)
  semi <- names(x)[semi] %-% ignore
  if(!length(semi))return(x)
  node <- .newcolname(base=nodebase,existing=names(x))
  x[node] <- NA
  x <- shuffle(x, names(x)[names(x) %contains% glue('^',nodebase,'\\b')])
  for(column in semi){
    disposable <- all( is.na( x[ node ][ is.defined( x[ column ] ) ] ) )
    if(disposable) x[ node ][ is.defined( x[ column ] ) ] <- column
    if(disposable) x[column] <- NULL
  }
  if(any(semi %in% names(x)))x <- .xmlSimplify(
  	x, 
  	stack = stack, 
  	ignore = ignore, 
  	nodebase = nodebase, 
  	...
  )
  x
}

.xpath2frame <- function(x, doc, fun=xmlValue, simplify = TRUE, namespaces = FALSE, ...){
  stopifnot(length(x)==1, length(doc)==1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces,...)
  index <- .xpath2index(x, doc,...)
  value <- .xpath2vector(x, doc, fun=xmlValue,...)
  stopifnot(nrow(index) == length(value))
  result <- cbind(index, value, stringsAsFactors = FALSE)
  if(local)free(doc)
  if(simplify) result <- .xmlSimplify(result)
  return(result)
}

xpath2frame <- function(x, doc, simplify = TRUE, sort = TRUE, nodebase = 'node', namespaces = FALSE, ...){
  stopifnot(length(doc)==1)
  local <- !inherits(doc,'XMLInternalDocument')
  file <- local && is.character(doc) && file.exists(doc)
  if(local) doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces)
  result <- lapply(x,.xpath2frame, doc = doc, simplify = FALSE, ...)
  if(local)free(doc)
  result <- metaMerge(result)
  result <- shuffle(result, 'value',after=NULL)
  if(simplify) result <- .xmlSimplify(result, stack = TRUE, nodebase = nodebase, ...)
  result <- as.keyed(result, key = names(result) %-% 'value')
  if(sort) result <- sort(result)
  return(result)
}
.parse <- function(doc, asText, error, namespaces,...){
	doc <- xmlParse(doc, asText = asText, error=error,...)
	if(!namespaces){
		removeXMLNamespaces(xmlRoot(doc), all = TRUE)
		doc <- xmlParse(saveXML(doc), asText = TRUE, error = function(...){})
	}
	doc
}
xlog <- function(
  run, 
  project = getwd(), 
  rundir = filename(project,run), 
  file= filename(rundir,run,'.xml'), 
  xpath=c(
      '/output/nonmem/problem/problem_title',
      '/output/nonmem/problem/estimation/estimation_method',
      '/output/nonmem/problem/estimation/estimation_title',
      '/output/nonmem/problem/estimation/termination_status',
      '/output/nonmem/problem/estimation/final_objective_function',
      '/output/nonmem/problem/estimation/theta/val',
      '/output/nonmem/problem/estimation/omega/row/col',
      '/output/nonmem/problem/estimation/sigma/row/col'
  ),
  includeFile = NULL,
  simplify = TRUE,
  sort = TRUE,
  nodebase = 'node',
  namespaces = FALSE,
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
  result <- lapply(
	file, 
	function(doc) xpath2frame(
		x=xpath, 
		doc=doc, 
		simplify = FALSE, 
		sort = FALSE, 
		nodebase = nodebase,
		namespaces = namespaces,
		...
	)
  )
  fnm <- .newcolname(base = 'file', existing=names(result))
  rnm <- .newcolname(base = 'run', existing=names(result))
  identify <- function(i){
    dat <- result[[i]]
    if(mrun || includeFile) dat[fnm] <- file[[i]]
    if(!mrun) dat[rnm] <- run[[i]]
    dat
  }
  result <- lapply(seq_along(result), identify)
  result <- metaMerge(result)
  if(simplify) result <- .xmlSimplify(result, stack = TRUE, ignore = c(fnm,rnm), nodebase = nodebase, ...)
  result <- shuffle(result, c(rnm,fnm) %n% names(result))
  result <- shuffle(result, 'value',after=NULL)  
  result <- as.keyed(result, key = names(result) %-% 'value')
  if(sort) result <- sort(result)
  return(result)
}




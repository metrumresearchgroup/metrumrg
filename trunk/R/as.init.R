# We need two object types:  theta list, and individual list members.
# Class 'theta' will be a list of theta members, possibly with attributes.
# Theta members will be vectors: low init up, with a fixed attribute.
# Need get/set functions for individual members, and possibly for list as whole.
# Need to support subset and element select on list.
# Need to support conversion to character.
# Need to support get/set fixed status.

as.init <- function(x,...)UseMethod('as.init')
as.init.numeric <- function(x=numeric(0),fixed=FALSE,comment=character(0),...){
	#x may be one,two, or three values
	#init, low/init, or low/init/up
	stopifnot(length(x)<=3,is.logical(fixed),inherits(comment,'character'))
	y <- c(-Inf,NA,Inf)
	names(y) <- c('low','init','up')
	class(y) <- c('init',class(y))
	if(length(x)==1)y['init'] <- x
	if(length(x)==2)y[c('low','init')] <- x
	if(length(x)==3)y[c('low','init','up')] <- x
	if(is.na(y['low']))y['low'] <- -Inf
	if(is.na(y['up']))y['up'] <- Inf
  if(y['low'] > y['up']) stop('lower bound must not be greater than upper bound')
  if(!is.na(y['init']))stopifnot(y['low'] <= y['init'],y['init'] <= y['up'])
  if(fixed & is.na(y['init']))stop('initial cannot be fixed if missing')
  if(!fixed & !is.na(y['init']) & y['init']==0)stop('initial cannot be fixed to zero')
	if(fixed) y[c('low','init','up')] <- y['init']
  if(length(comment))comment(y) <- comment
	y
}
as.character.init <- function(x,...){
  fixed <- x['low']==x['init'] & x['init']==x['up']
  com <- comment(x)
  if(is.na(fixed))fixed <- FALSE
  if(!is.na(x['init']) & all(is.infinite(x[c('low','up')]))) x <- x['init']
  if(!is.na(x['init']) & is.infinite(x['up'])) x <- x[c('low','init')]
  x[] <- sapply(x,toupper)
  x[is.na(x)] <- ''
  if(fixed) x <- x['init']
  len <- length(x)
  y <- paste(x,collapse=',')
  if(len>1) y <- parens(y)
  if(fixed) y <- paste(y,'FIXED')
  if(!is.null(com)) {
    com <- as.character(com)
    if(length(com)==1)y <- paste(y,com, sep='; ')
    if(length(com)>1){
      com <- paste('; ',com)
      y <- c(y,com)
    }
  }
  y
}
format.init <-function(x,...)as.character(x,...)
print.init <-function(x,...)print(format(x,...))
fixed <- function(x,...)UseMethod('fixed')
`fixed<-` <- function(x,value)UseMethod('fixed<-')
fixed.init <- function(x,...)!any(is.na(x)) & length(unique(x)) == 1
`fixed<-.init` <- function(x,value){
  stopifnot(is.logical(value))
  if(is.na(value))stop('NA found where logical required')
  if(fixed(x)==value)return(x)
  if(is.na(x['init']))stop("cannot alter 'fixed' for a missing value")
  if(value)x[c('low','init','up')] <- x['init']
  else{
    #user has requested 'not fixed' on something that is 'fixed'
    x['low'] <- -Inf
    x['up'] <- Inf
  }
  x
}

as.initList <- function(x,...)UseMethod('as.initList')
as.initList.list <- function(x,...){
  stopifnot(length(x)>0)
  is.init <- sapply(x,inherits,'init')
  class(x) <- c('initList',class(x))
  x
}
as.character.initList <- function(x,...)c('',unlist(lapply(x,as.character)))
format.initList <-function(x,...)as.character(x,...)
print.initList <-function(x,...)print(format(x,...))

as.initList.character <- function(x,...){
  #each init starts on a particular line.  The comment on that same line, and
  #any pure comment to follow, will accompany.
  x <- sub('^[ \t]+','',x) #no leading whitespace
  x <- sub('[ \t]+$','',x) #no trailing whitespace
  x <- x[x!=''] #no blank lines
  dat <- cumsum(grepl('^[^;]',x))#test for leading nonsemicolon
  x <- x[dat>0] #discard lines before first data line
  commentIndex <- x %contains% ';' #
  dataIndex <- !grepl('^[;]',x) #first char is not semicolon
  comments <- .comments(x)#comments only
  data <- sub(';.*$','',x) # data only
  #data <- data[data!=''] # just data lines
  #comments <- split(comments,cumsum(dataIndex)) # number comment chunks like data lines
  if(length(data)==0)stop('no uncommented data to parse')
  data <- paste(data,collapse='\n')
  data <- gsub('[ \t]+',' ',data) #single space delimiters
  data <- gsub('FIXED','FIX',data)
  data <- gsub(' FIX','FIX',data) #only remaining spaces should be internal or external delimiters
  data <- gsub('INF','Inf',data)
  data <- unlist(strsplit(data,'(',fixed=TRUE))
  data <- unlist(.explicit(data))
  data <- unlist(strsplit(data,' '))
  data <- data[data!='']
  #data <- data[data!='\n']
  fixed <- data %contains% 'FIX'  
  data <- gsub('FIX','',data)
  line <- seq_along(dataIndex)
  line[!dataIndex] <- NA
  line <- locf(line)
  names(comments) <- line
  comments <- comments[comments!='']
  #line <- cumsum(data %contains% '\n')
  #data <- gsub('\n','',data)
  data <- strsplit(data,',')
  numreturns <- lapply(data,function(x)gregexpr('\n',x))
  numreturns <- lapply(numreturns, function(x)lapply(x,function(y)y>=0))
  numreturns <- lapply(numreturns, unlist)
  numreturns <- lapply(numreturns,sum)
  numreturns <- unlist(numreturns)
  cumreturns <- cumsum(numreturns)
  lastWindow <- cumreturns + 1
  ranges <- lapply(seq_along(numreturns),function(slot)seq(from=numreturns[[slot]],to=lastWindow[[slot]]))
  data <- lapply(data,as.numeric)
  stopifnot(length(data) == length(fixed))
  inits <- lapply(
    seq_along(data),
    function(i){
      d <- data[[i]]
      com <- as.character(comments[names(comments) %in% ranges[[i]]])
      f <- fixed[[i]]
      out <- as.init(x=d,fixed=f,comment=com)
      if(all(is.na(d)))out <- NULL
      out
    }
  )
  inits <- inits[!sapply(inits,is.null)]
  as.initList(inits)
}

.comments <- function(x)sub('^[^;]*;?(.*)$','\\1',x)
.space2comma <- function(x){
  if(!length(x))return(x)
  x[[1]] <- gsub(' ',',',x[[1]])
  x
}
.explicit <- function(x){
    halves <- strsplit(x,')',fixed=TRUE)
    halves <- lapply(halves,.space2comma)
  }
`[.initList` <- function (x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
as.initList.numeric <- function(x,fixed=FALSE,...){
  stopifnot(is.logical(fixed))
  fixed <- rep(fixed,length.out=length(x))
  y <- lapply(
    seq_along(x),
    function(i)as.init(x[[i]],fixed=fixed[[i]])
  )
  y <- as.initList(y)
  y
}
`$.init` <- function(x,name)x[[name]]
`$<-.init` <- function(x,name,value){
  if(!name %in% c('low','init','up'))stop('attempt to set an invalid init element')
  if(is.null(value))stop('attempt to delete a required init element')
  x[name] <- value
  x
}
fixed.initList <- function(x,...)sapply(x,fixed)
`fixed<-.initList` <- function(x,value){
	stopifnot(is.logical(value))
	value <- rep(value,length.out=length(x))
	for(i in seq_along(value))fixed(x[[i]]) <- value[[i]]
	x
}

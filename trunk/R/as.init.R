# We need two object types:  theta list, and individual list members.
# Class 'theta' will be a list of theta members, possibly with attributes.
# Theta members will be vectors: low init up, with a fixed attribute.
# Need get/set functions for individual members, and possibly for list as whole.
# Need to support subset and element select on list.
# Need to support conversion to character.
# Need to support get/set fixed status.

as.init <- function(x,...)UseMethod('as.init')
as.init.init <- function(x=numeric(0),fixed=FALSE,comment=character(0),...)as.init.numeric(x=x,fixed=fixed,comment=comment,...)
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
as.initList.list <- function(x,comment=character(0),...){
  stopifnot(length(x)>0,is.character(comment))
  is.init <- sapply(x,inherits,'init')
  class(x) <- c('initList',class(x))
  if(length(comment)) comment(x) <- comment
  x
}
as.character.initList <- function(x,...){
  com <- comment(x)
  if(is.null(com))com <- ''
  else com <- c('',paste(';',com))
  y <- c(com,unlist(lapply(x,as.character)))
  y
}
format.initList <-function(x,...)as.character(x,...)
print.initList <-function(x,...)print(format(x,...))
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
as.initList.numeric <- function(x,fixed=FALSE,comment=character(0),...){
  stopifnot(is.logical(fixed),is.character(comment))
  fixed <- rep(fixed,length.out=length(x))
  y <- lapply(
    seq_along(x),
    function(i)as.init(x[[i]],fixed=fixed[[i]])
  )
  y <- as.initList(y)
  if(length(comment)) comment(y) <- comment
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
.initcomments <- function(x)sub('^[^;]*;?(.*)$','\\1',x)
.initdata <- function(x)sub(';.*','',x)
.initEstimateNum <- function(x){
  # an estimate is initiated by a parenthesis, or by a qualifying character.
  # a qualifying character is a numeric or reserved char.
  # if initiated by parenthesis, it is closed by parenthesis, else by next
  # qualifying character
  # only 0123456789.-+eI( can initiate an estimate. For (, only ) can close it. For the others,
  # the first space or newline ends the numeric region.
  y <- numeric(0)
  openers <- c('0','1','2','3','4','5','6','7','8','9','.','-','+','(')
  closers <- c(' ','\n','\t') #or end of vector
  region <- 0
  parenthetical <- FALSE
  active <- FALSE
  for(i in x){
    if(!active){
      if(i %in% openers){
        region <- region + 1
        active <- TRUE
      }
    }else{
      if(parenthetical){
        if(i==')') active <- FALSE
      }else{
        if(i %in% closers) active <- FALSE
      }
    }
    if(i=='(')parenthetical <- TRUE
    if(i==')')parenthetical <- FALSE
    y <- append(y,region)
  }
  y
}

.initLineNum <- function(x){
  newline <- x=='\n'
  #I am the first character in a line if I follow a newline, or if I am the veryfirst character.
  frst <- prev(newline)
  frst[is.na(frst)] <- TRUE
  linenum <- cumsum(frst)
  linenum
}
as.initList.character <- function(x,...){
  stopifnot(length(x)>0)
  comments <- .initcomments(x)
  data <- .initdata(x)
  data <- paste(data,collapse='\n')
  z <- strsplit(data,NULL)[[1]]
  #classify each character in z as belonging to a particular line and particular estimate.
  est <- .initEstimateNum(z)
  line <- .initLineNum(z)
  inits <- split(z,est)
  inits <- lapply(inits,paste,collapse='')
  globalcom <- comments[unique(line[est==0])]
  if(0 %in% est) inits <- inits[-1]
  inits <- lapply(
    seq_along(inits),
    function(i){
      x <- inits[[i]]
      com <- comments[.initRelComment(i,est,line)]
      com <- com[com!='']
      comment(x) <- com
      x
    }
  )
  inits <- lapply(
    seq_along(inits),
    function(i){
      com <- comment(inits[[i]])
      if(is.null(com))com <- character(0)
      out <- .as.init.character(inits[[i]],comment=com)
      out
    }
  )
  as.initList(inits,comment=globalcom)
}
.initRelComment <- function(i,est,line){
  #need numbers for comment lines for any comment on my line, plus any trailing
  #comments, i.e. those after this line but before a new est is started
  #use comment groups: where runhead est coincides with runhead line
  stopifnot(length(est)==length(line))
  groupstart <- runhead(est) & runhead(line)
  group <- cumsum(groupstart)
  mygroup <- unique(group[est==i])#probably only 1
  mylines <- unique(line[group==mygroup])
  mylines
}
.as.init.character <- function(x,comment=character(0),...){#limited utility
  stopifnot(length(x)==1)
  fixed = FALSE
  if(x %contains% 'FIX|FIXED') fixed <- TRUE
  x <- gsub('FIXED','',x)
  x <- gsub('FIX','',x)
  x <- gsub('\n','',x)
  x <- sub('(','',x,fixed=TRUE)
  x <- sub(')','',x,fixed=TRUE)
  x <- gsub(' +',' ',x)
  x <- sub('^ ','',x)
  x <- sub(' $','',x)
  x <- gsub(',? ',',',x)
  x <- strsplit(x,',')[[1]]
  x <- as.numeric(x)
  x <- as.init(x,fixed=fixed,comment=comment,...)
  x
}
as.initList.initList <- function(x,...)x

tweak <- function(x,...)UseMethod('tweak')
tweak.init <- function(x,sd=0.13,digits=3,...){
	scale <- rnorm(1,mean=1,sd=sd)
	y <- x$init * scale
	y <- signif(y,digits=digits)
	if(y < x$low | y > x$up) return(tweak.init(x,sd=sd,digits=digits,...))
	x$init <- y
	x
}
	
tweak.initList <- function(x,sd=0.13,digits=3,...){
	x[] <- lapply(x,tweak,sd=sd,digits=digits,...)
	x
}
tweak.nmctl <- function(x,sd=0.13,digits=3,...){
	stopifnot('theta' %in% names(x))
	x$theta <- as.initList(x$theta)
	x$theta <- tweak(x$theta,sd=sd,digits=digits,...)
	x
}
	



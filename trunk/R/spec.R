library(metrumrg)
encode <- function(x,...)UseMethod('encode')
encode.list <- function(x,labels=NULL,sep='/',...){
  if(!is.null(labels) & length(labels) != length(x))stop('lengths of x and labels must match')
  sapply(seq_along(x),function(i)encode(as.character(x[[i]]),labels=labels[[i]],sep=sep,...))
}
encode.character <- function(x,labels=NULL,sep='/',...){
  if(!is.null(labels) & length(labels) != length(x))stop('lengths of x and labels must match')
  if(!is.null(labels))if(any(is.defined(labels)))x[is.defined(labels)] <- paste(x,labels,sep=sep)[is.defined(labels)]
  doublesep <- glue(sep,sep)
  x <- paste(x,collapse=doublesep)
  x <- glue(doublesep,x,doublesep)
  x
} 
encode.default <- function(x,labels=NULL,sep='/',...)encode(as.character(x),labels=labels,sep=sep,...)


###
a <- encode(
  x = list(
    c('M','F'),
    c(1:4)
  ),
  labels = list(
    c('male','female'),
    c('caucasian','asian','african',NA)
  )
)
b <- encode(c(1:2),c('pediatric','adult'))
a
b

###


.encoded <- function(x,...){
  stopifnot(length(x) == 1)
  x <- as.character(x)
  if(is.na(x))return(FALSE)
  if (nchar(x) < 5 )return(FALSE)
  first <- substr(x,1,1)
  second <- substr(x,1,1)
  end <- nchar(x)
  ultimate <- substr(x,end,end)
  penult <- substr(x, end - 1,end - 1)
  if(
    second != first ||
      ultimate != first ||
      penult != first
  )return(FALSE)
  return(TRUE)
}
encoded <- function(x, ...)UseMethod('encoded')
encoded.default <- function(x, ...)sapply(x,.encoded,USE.NAMES=FALSE)

ranged <- function(x,...)grepl('(\\(|\\[)[.1-9]*,[.1-9]*(\\)|\\])$',x,...)
ranged(c('foo','[4,8.2]','(,8.2]','(,]'))

###
encoded(a)
encoded(b)
c <- c('a',NA,'##b##')
encoded(c)
###

.extract <- function(x,node,...){
  stopifnot(length(x) == 1)
  x <- as.character(x)
  if(!encoded(x)) return(as.character(NA))
  sep <- substr(x,1,1)
  doublesep <- glue(sep,sep)
  y <- strsplit(x,split=doublesep,fixed=TRUE)[[1]]
  y <- y[y!='']
  lift <- function(x,node){
    z <- strsplit(x,split=sep,fixed=TRUE)[[1]]
    if(length(z) >= node) (z[[node]]) 
    else as.character(NA)
  }
  y <- sapply(y,lift,node=node)
  y <- as.character(y)
  y
} 

.extract('..1..',1)
.extract('..1.a..',2)
.extract('..1..',2)


.codes <- function(x,...).extract(x,node=1,...)
.decodes <- function(x,...).extract(x,node=2,...)

codes <- function(x,...)UseMethod('codes')
codes.default <- function(x, simplify = TRUE, ...){
  y <- lapply(x,.codes)
  if(length(y) == 1 & simplify) y <- y[[1]]
  y
}

decodes <- function(x,...)UseMethod('decodes')
decodes.default <- function(x, simplify = TRUE, ...){
  y <- lapply(x,.decodes)
  if(length(y) == 1 & simplify) y <- y[[1]]
  y
}
###
codes(a)
codes(b)
codes(b,simplify=F)
codes(c)
codes('..1..')
decodes(a)
decodes(b)
decodes(c)
###

as.spec <- function(x, ...)UseMethod('as.spec')
as.spec.data.frame <- function(x, ...){
  stopifnot(identical(names(x),c('column','label','type','guide','required','comment')))
  #x$guide <- as.guide(x$guide)
  x <- as.keyed(x,'column')
  class(x) <- 'spec' %u% class(x)
  x
}
read.spec <- function(x,...){
  x <- read.table(x,header=TRUE,as.is=TRUE,na.strings=c('','.','NA'), quote='',sep='\t')
  x <- as.spec(x)
  x
}
write.spec <- function(x,file,...)write.table(x,file=file, row.names=FALSE,quote=FALSE,na='.',sep='\t',...)
as.vector.spec <- function(x,mode='any',...) x$column
summary.spec <- function (object, ...) {
  x <- object
  z <- list()
  z$key <- key(x)
  z$naKeys <- sum(naKeys(x))
  z$dupKeys <- sum(dupKeys(x))
  suppressWarnings(z$unsorted <- sum(unsorted(x)))
  writeLines(paste(z$key, collapse = "~"))
  writeLines(paste(z$naKeys, "NA keys"))
  writeLines(paste(z$dupKeys, "duplicate keys"))
}
as.spec.character <- function(x,...){
  stopifnot(length(x) == 1 && file.exists(x))
  y <- read.spec(x)
  y
}

specification <- function(x,...)UseMethod('specification')
specification.default <- function(x, ...)x
specification.comment <- function(x,...)factor(x, levels=c(TRUE,FALSE), labels=c('C','.'))
.guide <- function(x,...)UseMethod('.guide')
.guide.default <- function(x,tol=10,sep='/',...){
  x <- as.character(x)
  codes <- unique(x)
  codes <- codes[is.defined(codes)]
  if(length(codes) <= tol) if(!any(codes %contains% sep))return(encode(codes,labels=codes,sep=sep))
  return(as.character(NA))
}
.guide.numeric <- function(x,...){
  if(all(x=round(x),na.rm=TRUE)) .quide(as.integer(x))
  else glue('[',min(x,na.rm=TRUE),',',max(x,na.rm=TRUE),']')
}
.guide.factor <- function(x,sep='/',...){
  codes <- levels(x)
  if(!any(codes %contains% sep)) encode(codes,labels=codes,sep=sep)
  else as.character(NA)
}
.guide.mDateTime <- function(x,...)'%Y-%m-%d %H:%M'
.guide.mDate <- function(x,...)'%Y-%m-%d'
.guide.mTime <- function(x,...)'%H:%M'
.type <- function(x,...)UseMethod('.type')
.type.default <- function(x,...) 'character'
.type.timepoint <- function(x,...)'datetime'
.type.numeric <- function(x,...)if(all(x==round(x),na.rm=TRUE)) 'integer' else 'numeric'
.label <- function(x,...)UseMethod('.label')
.label.default <- function(x,...){
  lab <- attr(x,which='label')
  if(is.null(lab)) lab <- as.character(NA)
  lab
}
.required <- function(x,...)UseMethod('.required')
.required.default <- function(x,...)as.integer(all(is.defined(x)))

specification.data.frame <- function(x,tol=10,sep='/',...){
  x[] <- lapply(x,specification)
  y <- data.frame(
    stringsAsFactors=FALSE,
    column=names(x),
    label=sapply(x,.label,...),
    type=sapply(x,.type,...),
    guide=sapply(x,.guide,tol=tol,sep=sep,...),
    required=sapply(x,.required,...),
    comment=NA
  )
  y$label[is.na(y$label)] <- y$column[is.na(y$label)]
  rownames(y) <- NULL
  y <- as.spec(y)
  y
}

`%matches%` <- function(x, y, ...)UseMethod("%matches%")
`%matches%.spec` <- function(x,y,...) y %matches% x
`%matches%.character` <- function(x, y, ...){
  stopifnot(length(x) == 1)
  if(! file.exists(x))stop(x,' not found')
  x <- read.csv(x,as.is=TRUE,na.strings=c('','.','NA'))
  x %matches% y
}
`%matches%.data.frame` <- function(x, y, ...)as.keyed(x) %matches% y
`%matches%.keyed` <- function(x, y, ...){
  y <- as.spec(y)
  x[] <- lapply(x,specification)
  unspecified <- x %-% y
  if(length(unspecified)){
    message('unspecified: ',paste(unspecified,collapse=', '))
    return(FALSE)
  }
  unimplemented <- y %-% x
  if(length(unimplemented)){
    message('unimplemented: ',paste(unimplemented,collapse=', '))
    return(FALSE)
  }
  if(any(as.vector(x) != as.vector(y))){
    message('column order differs, e.g. ',as.vector(x)[as.vector(x) != as.vector(y)][[1]])
    return(FALSE)
  }
  for(col in as.vector(y)[y$type == 'integer'])if(any(na.rm=TRUE, x[[col]] != as.integer(x[[col]]))){
    message(col,' not integer')
    return(FALSE)
  }
  for(col in as.vector(y)[y$type == 'numeric'])if(!is.numeric(x[[col]])){
    message(col,' not numeric')
    return(FALSE)
  }
  for(col in y$column[y$type == 'datetime']){
    format <- y$guide[y$column == col]
    if(any(
      !is.na(x[[col]]) & is.na(as.mDateTime(format=format,x[[col]]))
    )){
      message(col, ' not (coercible to) ', format)
      return(FALSE)
    }
  }
  z <- y[encoded(y$guide),]
  allgoodcodes <- TRUE
  for(col in z$column){
    codes <- codes(z$guide[z$column==col])
    vals <- unique(x[,col])
    vals <- vals[is.defined(vals)]
    bad <- vals %-% codes
    if(length(bad)){
      message('For ',col,': expecting only NA or ',paste(codes, collapse=', '),
      ' but found ',paste(bad, collapse=', '))
      allgoodcodes <- FALSE
    }
  }
  if(!allgoodcodes)return(FALSE)
  for(col in y$column){
    condition <- y$required[y$column == col]
    required <- as.logical(eval(parse(text=condition), envir=x))
    required[is.na(required)] <- TRUE
    missing <- is.na(x[,col])
    exceptions <- sum(required & missing)
    if(exceptions)stop('found ',exceptions,' NA in ',col,' for condition ',condition)
  }
  TRUE
}
  

test <- Theoph
names(test) <- c('SUBJ','WT','DOSE','DATETIME','DV')
test$DATETIME <- as.mDateTime(as.mDate('2013-10-17'),as.mTime(as.second(round(as.minute(as.second(as.hour(test$DATETIME)))))))
test <- as.nm(test)
summary(test)
head(test)
spec <- specification(test)
summary(spec)
write.spec(spec,'test.spec')
write.nm(test, 'test.csv')
test %matches% spec
test %matches% 'test.spec'
'test.csv' %matches% spec
'test.csv' %matches% 'test.spec'
spec %matches% test
spec %matches% 'test.csv'
unlink('test.csv')
unlink('test.spec')
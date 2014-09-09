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
.encoded <- function(x,...){
  stopifnot(length(x) == 1)
  x <- as.character(x)
  x <- sub('^\\s','',x)
  x <- sub('\\s$','',x)
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
encoded.spec <- function(x,column=x$column,...)encoded(x$guide[x$column %in% column])
extract <- function(x, pattern, group = 0, invert=FALSE,...){
  y <- regexec(pattern,x)
  scale <- sapply(y,length)
  group <- rep(group, length.out= length(y))
  group <- group + 1
  start <- sapply(seq_along(y), function(i){
    y <- y[[i]]
    group <- group[[i]]
    if(group > length(y)) return(0)
    y[[group]]
  })
  start[is.na(start)] <- 0
  start[start < 0] <- 0
  len <- sapply(seq_along(y), function(i){
    y <- y[[i]]
    group <- group[[i]]
    if(group > length(y)) return(0)
    attr(y,'match.length')[[group]]
  })
  len[is.na(len)] <- 0
  len[len < 0] <- 0
  stop <- start + len - 1
  stop[stop < 0] <- 0
  indices <- lapply(seq_along(y),function(i)start[[i]]:stop[[i]])
  indices[len == 0] <- 0
  splits <- strsplit(x,NULL)
  welds <- sapply(seq_along(splits), function(i){
    z <- splits[[i]]
    index <- indices[[i]]
    if(invert){
      if(len[i] > 0) z <- z[-index]
    } else z <- z[index]
    z <- paste(z,collapse='')
    z
   })
  welds[is.na(x)] <- NA
  welds
}
guidetext <- function(x,...)UseMethod('guidetext')
guidetext.spec <- function(x,column=x$column,...){
  x <- x[x$column %in% column,]
  pattern <- '((\\(|\\[) *([-+eE.0-9]*) *, *([-+eE.0-9]*) *(\\)|\\])) *$'
  y <- extract(x$guide,pattern=pattern,invert=TRUE)
  y <- sub('^\\s','',y)
  y <- sub('\\s$','',y)
  y[encoded(y)] <- NA
  y[y == ''] <- NA
  y[x$type == 'datetime'] <- NA
  y
}
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
.codes <- function(x,...).extract(x,node=1,...)
.decodes <- function(x,...).extract(x,node=2,...)
codes <- function(x,...)UseMethod('codes')
codes.default <- function(x, simplify = TRUE, ...){
  y <- lapply(x,.codes)
  if(length(y) == 1 & simplify) y <- y[[1]]
  y
}
codes.spec <- function(x,column=x$column,...)codes(x$guide[x$column %in% column])
decodes <- function(x,...)UseMethod('decodes')
decodes.default <- function(x, simplify = TRUE, ...){
  y <- lapply(x,.decodes)
  if(length(y) == 1 & simplify) y <- y[[1]]
  y
}
decodes.spec <- function(x,column=x$column,...)decodes(x$guide[x$column %in% column])
labels.spec <- function(object,column=object$column,...)object$label[object$column %in% column]
as.spec <- function(x, ...)UseMethod('as.spec')
as.spec.data.frame <- function(x, ...){
  expected <- c('column','label','type','guide','required','derivation')
  found <- names(x)
  missing <- expected %-% found
  extra <- found %-% expected
  if(length(missing))warning('missing expected column(s) ',paste(missing,collapse=', '))
  if(length(extra))message('found unexpected column(s) ',paste(extra,collapse=', '))
  x <- as.keyed(x,'column')
  class(x) <- 'spec' %u% class(x)
  x
}
read.spec <- function(x, unquote = TRUE, ...){
  x <- read.table(x,header=TRUE,as.is=TRUE,na.strings=c('','.','NA'), quote='',sep='\t')
  chars <- sapply(x, inherits, 'character')
  if(unquote) x[chars] <- lapply(x[chars], function(col){
  	  col <- sub("^[\"']+",'',col)
  	  col <- sub("[\"']+$",'',col)
  })
  x <- as.spec(x)
  x
}
write.spec <- function(x,file,...)write.table(x,file=file, row.names=FALSE,quote=FALSE,na='.',sep='\t',...)
clean.spec <- function(file,...)write.spec(read.spec(file,...),file=file,...)
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
  if(all(x == round(x),na.rm=TRUE)) .guide(as.integer(x))
  else glue('[',min(x,na.rm=TRUE),',',max(x,na.rm=TRUE),']')
}
.guide.integer <- function(x,tol=10,...){
  if(length(unique(x)) <= tol) .guide(as.factor(x))
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
    derivation=NA
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
  allrequired <- TRUE
  for(col in y$column){
    condition <- y$required[y$column == col]
    required <- as.logical(eval(parse(text=condition), envir=x))
    required[is.na(required)] <- TRUE
    missing <- is.na(x[,col])
    exceptions <- sum(required & missing)
    if(exceptions){
      message('found ',exceptions,' NA in ',col,' for condition ',condition)
      allrequired <- FALSE
    }
    if(!allrequired)return(FALSE)
  }
  pattern <- '((\\(|\\[) *([-+eE.0-9]*) *, *([-+eE.0-9]*) *(\\)|\\])) *$'
  y$lo <- extract(y$guide,pattern,group=3)
  y$hi <- extract(y$guide,pattern,group=4)
  y$lo <- as.numeric(y$lo)
  y$hi <- as.numeric(y$hi)
  y$oplo <- extract(y$guide,pattern,group=2)
  y$ophi <- extract(y$guide,pattern,group=5)
  y$lo[y$lo == ''] <- NA
  y$hi[y$hi == ''] <- NA
  y$oplo[y$oplo == ''] <- NA
  y$ophi[y$ophi == ''] <- NA
  y$mn <- sapply(y$column,function(col)if(is.numeric(x[[col]]))min(x[[col]],na.rm=TRUE) else NA)
  y$mx <- sapply(y$column,function(col)if(is.numeric(x[[col]]))max(x[[col]],na.rm=TRUE) else NA)
  y$goodlo <- TRUE
  y$goodhi <- TRUE
  y$goodlo[with(y, is.defined(lo) & is.defined(oplo) & is.defined(mn) & oplo=='(' & mn <= lo)] <- FALSE
  y$goodlo[with(y, is.defined(lo) & is.defined(oplo) & is.defined(mn) & oplo=='[' & mn <  lo)] <- FALSE
  y$goodhi[with(y, is.defined(hi) & is.defined(ophi) & is.defined(mx) & ophi==')' & mx >= hi)] <- FALSE
  y$goodhi[with(y, is.defined(hi) & is.defined(ophi) & is.defined(mx) & ophi==']' & mx >  hi)] <- FALSE
  y$interval <- extract(y$guide,pattern, group=1)
  y$msg <- with(y, paste(column,'range',mn,',',mx,'outside of interval',interval,'\n'))
  y$failed <- !y$goodlo | !y$goodhi
  if(any(y$failed)){
    message(y$msg[y$failed])
    return(FALSE)
  }  
  TRUE
}

specfile <- function(
  run,
  project=getwd(),
  rundir = filename(project,run), 
  ctlfile = filename(rundir, run, ".ctl"),
  ...
){
  if(!missing(run))run <- as.character(run)
  if(!missing(rundir))rundir <- as.character(rundir)
  if(missing(run) & missing(rundir) & missing(ctlfile))stop('one of run, rundir, or ctlfile must be supplied')
  if(missing(run) & missing(ctlfile)) run <- basename(rundir)      
  if(missing(run) & missing(rundir))run <- sub('[.][^.]+$','',basename(ctlfile))
  if(missing(project) & !missing(rundir))project <- dirname(rundir)
  if(missing(project) & missing(rundir) & !missing(ctlfile))project <- dirname(dirname(ctlfile))
  control <- read.nmctl(ctlfile)
  dname <- getdname(control)
  datafile <- resolve(dname,rundir)
  specfile <- sub('\\.csv$','.spec',datafile)
  specfile
}
# need a function to clean up quotations, whitespace, update ranges, etc


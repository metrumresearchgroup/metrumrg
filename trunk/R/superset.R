#Although values were not calculated for dropped rows, they could be imputed if 
#the result is static on key, or leading subsets of key. 
.restore <- function(x,dropped,...){ # need to add records wherever dropped is TRUE
  stopifnot(is.data.frame(x),is.logical(dropped))
  if(any(is.na(dropped)))stop['dropped must not contain NA']
  if(sum(!dropped)!=nrow(x))warning('row count does not match sum of non-dropped')
  index <- rep(NA,length(dropped))
  index[!dropped] <- rownames(x)
  x[index,,drop=FALSE]
}
.distill <- function(x,known=character(0),...){ # x is a list of data frames
    stopifnot(is.list(x))
    if(!length(x))return(x)
    y <- x[[1]]
    stopifnot(is.data.frame(y))
    y <- y[unique(names(y))] # drop internal duplicates 
    y <- y[!names(y) %in% known] # drop external duplicates
    known <- c(known,names(y))
    c(list(y),.distill(x[-1],known=known,...)) # recursion
}
.markup <- function(lst,key,...){ # lst is a list of data frames
    stopifnot(is.list(lst),!!length(lst),is.data.frame(lst[[1]]))
    x <- lst[[1]]
    lst <- lst[-1]
    if(!length(lst))return(x)
    y <- lst[[1]]
    lst <- lst[-1]
    ind <- key[key %in% names(y)]
    y <- as.keyed(y,ind)
    x$metrumrg.markup <- 1:nrow(x)
    x <- as.keyed(x,'metrumrg.markup')
    z <- x^y
    if(any(is.na(z$metrumrg.markup)))stop('pseudo rows introduced')
    z <- sort(as.keyed(z,'metrumrg.markup'))
    z$metrumrg.markup <- NULL
    z <- as.data.frame(z,...)
    .markup(c(list(z),lst),key=key,...)
}
.superbind <- function(lst,i=0,exclusive=NULL){
    stopifnot(is.list(lst),!!length(lst),is.data.frame(lst[[1]]))
    x <- lst[[1]]
    i <- i + 1
    lst <- lst[-1]
    if(!length(lst))return(x)
    y <- lst[[1]]
    lst <- lst[-1]
    if(nrow(y) < nrow(x)){
      message('ignoring table ',i,': expected ', nrow(x),' rows but found ',nrow(y),'.')
      return(x)
    }
    if(nrow(y) %% nrow(x) != 0){
      message('ignoring table ',i,': expected ', nrow(x),' rows but found ',nrow(y),' (not a multiple).')
      return(x)
    }
    analogs <- intersect(names(x),names(y))
    #(implicitly, y cols with new names are informative.)
    informative <- function(x,y){
      stopifnot(length(x)==length(y))
      # first, we detect informative elements of y
      generated <- is.na(x) & !is.na(y) & y!=0 #y:0 is probably just NONMEM substitute for NA
      degenerated <- !is.na(x) & is.na(y)
      altered <- !is.na(x) & !is.na(y) & x!=y
      any(generated | altered)
    }
    index <- sapply(analogs, function(col)informative(x[[col]],y[[col]]))
    goodDups <- analogs[index]
    badDups <- setdiff(analogs,goodDups)
    newname <- function(x,i)glue(names(x),'.',i)
    
    #every analog y column will be renamed or dropped (or both).
    if(is.character(exclusive)) y <- y[,!names(y) %in% exclusive,drop=FALSE]
    else{
      if(is.null(exclusive)) y <- y[,!names(y) %in% badDups,drop=FALSE]
      else if(as.logical(exclusive)) y <- y[,!names(y) %in% goodDups,drop=FALSE]
    }
    fix <- names(y) %in% analogs
    names(y)[fix] <- map(names(y)[fix],from=analogs,to=newname(analogs))
    z <- cbind(x,y)
    .superbind(c(list(z),lst), i=i, exclusive=exclusive)
}
# Need a function that, given a nonmem run directory, merges the output with the input.
superset <- function(
  run,
  project=getwd(),
  rundir = filename(project,run), 
  ctlfile = filename(rundir, run, ".ctl"),
  #key=c('ID','DATE','TIME','CMT'),
  key=character(0),
  #convert=FALSE,
  read.input=list(read.csv,header=TRUE,as.is=TRUE,na.strings='.'),
  read.output=list(read.table,header=TRUE,as.is=TRUE,skip=1,comment.char='',check.names=FALSE),
  exclusive=NULL,
  as.logical=FALSE,
  ...
){
  #functions
  revert <- function(x,labels,analogs){
    fix <- names(x) %in% labels
    names(x)[fix] <- map(names(x)[fix],from=labels,to=analogs)
    x
  }
  tablePaths <- function(tables)sapply(
    seq_along(tables), 
    function(recnum)tryCatch(
      extfile(
        tables[[recnum]],
        dir=rundir,
        extreg='FILE'
      ),
      error=function(e)warning('in table ',recnum,': ',e,call.=FALSE,immediate.=TRUE)
    )
  )
  read.any <- function(file,args){
    fun <- match.fun(args[[1]])
    args <- args[-1]
    args <- c(args,file=file)
    do.call(fun,args)
  }
  agree <- function(x,expected)if(nrow(x)!=expected)warning('expected ',expected,' rows but found ',nrow(x))
  # process
  stopifnot('header' %in% names(read.input))
  if(missing(rundir))rundir <- dirname(ctlfile)
  if(rundir!=dirname(ctlfile))warning('rundir does not specify parent of ctlfile')
  control <- read.nmctl(ctlfile)
  dname <- getdname(control)
  datafile <- resolve(dname,rundir)
  if (!file.exists(datafile))stop(dname, " not visible from ", rundir, call. = FALSE)
  outputdomain <- names(control) %contains% "tab"
  tables <- control[outputdomain]
  paths <- tablePaths(tables)
  #Now we have input path (datafile) and output paths (paths).  We munge them together.
  #But first, we need to decode the INPUT statement.
  labels <- .nminput(control)
  input <- read.any(file=datafile,args=read.input)
  lines <- readLines(datafile)
  #To guide merging, we need to know which records from the original data set were dropped.
  #Character and numeric versions of the data may differ in row count if there was a header.
  dropped <- .nmdropped(data=input,lines=lines,test=.nmignore(control),labels=labels)
  if(as.logical)return(dropped)
  if(!length(paths)){message('nothing to add');return(input)}
  if(length(labels)>ncol(input))stop('more nonmem aliases than data columns')
  output <- lapply(paths, read.any, args=read.output)
  analogs <- names(input)[seq_along(labels)]
  output <- lapply(output,revert,labels=labels,analogs=analogs)
  #Now all the tables have corresponding column names.
  expected <- nrow(input) - sum(dropped)
  lapply(output,agree,expected)
  if(length(key)) return(.markup(lst=c(list(input),output),key=key))
  output <- .distill(output)
  output <- lapply(output,.restore,dropped=dropped)
  res <- .superbind(c(list(input),output),exclusive=exclusive)
  res[run] <- as.integer(dropped)
  res
}
#debug(superset)
#superset(ctlfile='../../model/abeta/1249/1249.ctl')

.nmdropped <- function(data,lines,test,labels,...){
  #data is the original data set as a data frame
  #lines is original data set, with any header, as character
  #test is a list corresponding to the INPUT options named ignore or accept
  #labels is the column names as described to NONMEM in the INPUT statement
  #lines may include header; we limit to no longer than data to discard header; 
  #it certainly would have been discarded (if present) by ignore logic.
  keep <- seq(length.out=nrow(data))
  lines <- rev(rev(lines)[keep])
  c1 <- substr(lines,1,1)
  cn <- sub(' *','',lines)
  cn[nzchar(cn)] <- substr(cn[nzchar(cn)],1,1)
  test <- lapply(test, .nmconditional, data=data, c1=c1, cn=cn,labels=labels,...)
  for(i in seq_along(test))if(names(test)[[i]]=='accept')test[[i]] <- !test[[i]] # convert accept to ignore
  .or(test)
}

.nmconditional <- function(x,data,c1,cn,labels,...){
  evalchar <- function(x,c1,cn,...){
    if(x=='@')return(cn %in% c(letters,LETTERS,'@'))
    else return(c1==x)
  }
  evalcond <- function(x,data,labels,...){
    label <- x['label']
    op <- x['operator']
    val <- x['value']
    if(is.na(label))stop('no label')
    if(is.na(val))stop('no value')
    if(is.na(op))op <- 'EQ'
    pos <- match(label,labels)
    vec <- data[pos]
    if(any(is.na(vec))){
    	    warning('imputing NA as "."')
    	    vec[is.na(vec)] <- '.'
    }
    if(!op %in% c('EQ','NE')){
      val <- as.numeric(val)
      vec <- as.numeric(vec)
    }
    op <- map(
      op, 
      from=c('EQ','NE','GT','GE','LT','LE'),
      to=  c('==','!=', '>','>=','<' ,'<=')
    )
    fun <- match.fun(op)
    fun(vec,val)
  }
  evalEither <- function(x,data,c1,cn,labels,...){
    if(length(x)==1)evalchar(x['value'],data=data,c1=c1,cn=cn,...)
    else evalcond(x,data=data,labels=labels,...)
  }
  result <- lapply(x,evalEither,data=data,c1=c1,cn=cn,labels=labels,...)
  .or(result)
}

.or <- function(x,na.rm=FALSE,...){
  x <- as.matrix(as.data.frame(x,...),...)
  apply(x,MARGIN=1,any,na.rm=na.rm)
}
.and <- function(x,na.rm=FALSE,...){
  x <- as.matrix(as.data.frame(x,...),...)
  apply(x,MARGIN=1,all,na.rm=FALSE)
}
  
#debug(.nmdropped)

.nminput <- function(x,...)UseMethod('.nminput')
.nminput.default <- function(x,...){
  if (!file.exists(x)) stop(x, " not found", call. = FALSE)
  control <- read.nmctl(x)
  .nminput(control)
}
.nminput.nmctl <- function(x,...){
  if(! 'input' %in% names(x))stop('no input record found in ',x,call.=FALSE)
  x <- x$input
  x <- paste(x,collapse=' ')
  x <- gsub(',',' ',x)
  x <- gsub(' +',' ',x)
  reserved=c(
    'ID','L1','L2','DV','MDV','RAW_','MRG_','RPT_',
    'TIME','DATE','DAT1','DAT2','DAT3','DROP','SKIP',
    'EVID','AMT','RATE','SS','II','ADDL','CMT','PCMT','CALL','CONT',
    'XVID1','XVID2','XVID3','XVID4','XVID5'
  )
  x <- gsub('DROP=','',x)
  x <- gsub('SKIP=','',x)
  x <- gsub('=DROP','',x)
  x <- gsub('=SKIP','',x)
  labels <- strsplit(x,' ')[[1]] # x is length one, so we keep just the first list
  labels <- lapply(labels,strsplit,'=')
 labels <- lapply(labels,unlist)
 labels <- sapply(
    labels,
    function(pair){
      if(length(pair)==1) return(pair[[1]])
      if(length(pair)>2)stop('too many synonyms')
      if(pair[[2]] %in% reserved) return(pair[[1]])#At least one must be reserved, per NONMEM help
      if(pair[[1]] %in% reserved) return(pair[[2]])#Prefer the first if both reserved (maybe does not occur if SKIP/DROP are removed).
      stop('unrecognized label syntax')
    }
  )
  #We now have a vector of labels as they will be used (if at all) in NONMEM output.
  #It is as long as the number of columns read from the input data set.
  labels
}
#debug(.nminput.nmctl)
#.nminput('../../model/abeta/1249/1249.ctl')

.nmignore <- function (x,...)UseMethod('.nmignore')
.nmignore.default <- function(x,...){
  if (!file.exists(x)) stop(x, " not found", call. = FALSE)
  control <- read.nmctl(x)
  .nmignore(control)
}
.nmignore.nmctl <- function(x,...){
  opt <- .nmdataoptions(x,...) # named character
  if(!length(opt))opt  <- c(ignore="#") # the nonmem default
  test <- opt[names(opt) %in% c('ignore','accept')]
  #Only accept list or ignore list should occur, but not both. Ignore character may occur regardless.
  #logic <- TRUE
  #if('accept' %in% names(opt)) logic <- !logic
  #The next character is (, @, or some other character.
  #We need to reduce the lists to canonical sets of exclusions.
  #Exclusions are either character or conditional.
  test <- lapply(test,.ignorecanonical)
  test # a list
}
.ignorecanonical <- function(x,...){ # scalar character
  x  <- sub('^\\(','',x)
  x <- sub('\\)$','',x)
  x <- strsplit(x,',',fixed=TRUE)[[1]] #only need the first, since x was scalar
  #now x is a chararcter vector of conditions (possibly scalar)
  #We now have character tests, or comparisons in the form 
  #label=value or label="value" or label='value' or label.op.value or label.value etc.
  x <- sub('=','.',x)
  x <- lapply(x,.ignorecondition)
  x
}
.ignorecondition <- function(x,...){ # a single condition of the form value or label.value or label.op.value
  x <- strsplit(x,'.',fixed=TRUE)[[1]]
  stopifnot(length(x) %in% c(1:3))
  #label the list members
  if(length(x)==1)names(x) <- 'value'
  if(length(x)==2)names(x) <- c('label','value')
  if(length(x)==3)names(x) <- c('label','operator','value')
  #remove quotes from values
  x['value'] <- sub('^"','',x['value'])
  x['value'] <- sub('"$','',x['value'])
  x['value'] <- sub("^'",'',x['value'])
  x['value'] <- sub("'$",'',x['value'])
  x
}
#debug(.nmignore.nmctl)
#.nmignore('../../model/abeta/1249/1249.ctl')
.nmdataoptions <- function(x,...)UseMethod('.nmdataoptions')
.nmdataoptions.default <- function(x,...){
  if (!file.exists(x)) stop(x, " not found", call. = FALSE)
  control <- read.nmctl(x)
  .nmdataoptions(control)
}
.nmdataoptions.nmctl <- function(x,...){
  if (!"data" %in% names(x))stop("data record not found in control stream")
  rec <- x$data
  rec <- sub(';.*','',rec)
  rec <- paste(rec,collapse=' ') # now scalar
  rec <- gsub('[[:space:]]+',' ',rec)
  reserved <- c(
    'IGNORE','NULL','ACCEPT','NOWIDE','WIDE','CHECKOUT',
    'RECORDS','LREC','NOREWIND','REWIND','NOOPEN','LAST20',
    'TRANSLATE','BLANKOK'
  )
  for(word in reserved) rec <- gsub(word,glue('$',word),rec,fixed=TRUE)
  splits <- strsplit(rec,'$',fixed=TRUE)[[1]] # rec is scalar; interested in the first (only) element
  names(splits) <- sapply(splits,function(split)tolower(sub('^([a-zA-Z0-9]+).*$','\\1',split)))
  splits <- sapply(splits,sub,pattern='^[a-zA-Z0-9]+ *=? *',replacement='')
  names(splits)[!names(splits) %in% tolower(reserved)] <- NA
  splits <- sub('^[[:space:]]+','',splits)
  splits <- sub('[[:space:]]+$','',splits)
  splits # a character vector
}
#.nmdataoptions('../../model/abeta/1249/1249.ctl')
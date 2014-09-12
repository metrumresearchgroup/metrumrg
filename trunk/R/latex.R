#quoting from http://en.wikibooks.org/wiki/LaTeX/Absolute_Beginners ...
#"Anything in LaTeX can be expressed in terms of commands and environments."

#LaTeX Commands
#LaTeX commands are case sensitive, and take one of the following two formats:
# - They start with a backslash \ and then have a name consisting of letters only. Command names are terminated by a space, a number or any #other "non-letter".
# - They consist of a backslash \ and exactly one non-letter.
#Some commands need an argument, which has to be given between curly braces { } after the command name. Some commands support optional #parameters, which are added after the command name in square brackets [ ]. The general syntax is:
#\commandname[option1,option2,...]{argument1}{argument2}...

#LaTeX environments
#Environments in LaTeX have a role that is quite similar to commands, but they usually have effect on a wider part of the document. Their syntax #is:
#\begin{environmentname}
#text to be influenced
#\end{environmentname}
#Between the \begin and the \end you can put other commands and nested environments. In general, environments can accept arguments as well, but #this feature is not commonly used and so it will be discussed in more advanced parts of the document.
is.alpha <- function(x,...){
  stopifnot(is.character(x))
  font <- c(letters,LETTERS)
  x <- strsplit(x,'')
  sapply(x,function(vec)all(vec %in% font))
}
is.one.nonalpha <- function(x,...){
  stopifnot(is.character(x))
  font <- c(letters,LETTERS)
  (nchar(x)==1) & (!x %in% font)
}
is.latex.token <- function(x,...)is.alpha(x) | is.one.nonalpha(x)
spaces <- function(x)paste(rep(' ',x),collapse='')
tagvalue <- function(x,sep='=',collapse=',',...){
  stopifnot(is.list(x))
  y <- sapply(
    seq_along(x),
    function(num){
      tag <- names(x)[[num]]
      value <- x[[num]]
      if(!is.null(tag))
        if(!is.na(tag))
          if(tag != "")value <- paste(tag,value,sep=sep)
      value
    }
  )
  paste(y,collapse=collapse)
}
latex.options <- function(x,...){
  x <- as.list(x)
  x <- tagvalue(x)
  if(nchar(x)) x <- glue('[',x,']')
  x
}
latex.args <- function(x,...){
  x <- sapply(x,function(x)glue('{',x,'}'))
  x <- paste(x,collapse='')
  x
}	
command <- function(x, options=NULL, args=NULL,depth=0){
  stopifnot(length(x)==1, is.latex.token(x))
  options <- latex.options(options)
  args <- latex.args(args)
  res <- paste(c(spaces(depth),'\\',x,options,args),collapse='')
  res
}
wrap <- function(x,environment,options=NULL,args=NULL,depth=0){
  stopifnot(is.character(x),length(environment)==1,is.latex.token(environment))
  options <- latex.options(options)
  args <- latex.args(args)
  begin <- glue(spaces(depth),'\\begin{',environment,'}',options,args)
  end   <- glue(spaces(depth),'\\end{',environment,'}')
  x <- glue(spaces(depth+1),x)
  res <- c(begin,x,end)
  res
}
breaks <- function(x,...)as.integer(runhead(x))[-1]
tabularformat <- function(justify,breaks,walls){
  stopifnot(
    length(walls)==2,
    length(justify)==length(breaks)+1,
    is.numeric(breaks),
    is.numeric(walls),
    !any(is.na(breaks)),
    !any(is.na(walls))
  )
  format <- ''
  format <- append(format, rep('|',walls[[1]]))
  for(i in seq_along(breaks)){
    format <- append(format,justify[[i]])
    format <- append(format,rep('|',breaks[[i]]))
  }
  #since there is one more justify than breaks ...
  format <- append(format, rev(justify)[[1]])
  format <- append(format, rep('|',walls[[2]]))
  format <- paste(format,collapse='')	
  format
}
row2tabular <- function(x,...){
  x <- paste(x,collapse=' & ')
  x <- paste(x,'\\\\')
  x
}
align.decimal <- function(x,decimal.mark='.',...){
  x <- prettyNum(x)
  nodecimal <- !contains(decimal.mark,x,fixed=TRUE)
  x[nodecimal] <- glue(x[nodecimal],' ')
  splits <- strsplit(x,decimal.mark,fixed=TRUE)
  splits <- lapply(splits,function(x){if(length(x)==1)x[[2]]<-'';x})
  tails <- sapply(splits,function(x)nchar(x[[2]]))
  need <- max(tails) - tails
  while(any(need > 0)){
    x[need > 0] <- glue(x[need > 0],' ')
    need <- need - 1
  }
  x
}
padded <- function (x, width = 4, ...) sprintf(glue("%0",width,".0f"), x)
as.ltable <- function(x,...)UseMethod('ltable')
ltable <- function(x,...)UseMethod('ltable')
ltable.data.frame <- function(
  x,
  caption=NULL,
  cap=caption,
  cap.top=TRUE,
  label=NULL,
  options='!htpb',
  environments='center',
  source=NULL,
  file=NULL, ### file needs to support verbatim e.g. _
  source.label='source: ',
  file.label='file: ',
  basefile=FALSE,
  footnote.size='tiny',
  ...
){
  x <- tabular(x, ...)
  if(!is.null(source))if(!is.null(source.label)) x <- c(x,glue('\\\\{\\',footnote.size,' ',source.label,source,'}'))  
  if(!is.null(file))if(!is.null(file.label)) x <- c(
    x,
    glue(
      '\\\\{\\',footnote.size,' ',
      file.label,
      if(basefile)basename(file) else file,
      '}'
    )
  )
  for (env in environments) x <- wrap(x,env)
  if (!is.null(label))label <- command('label',args=label)
  if(!is.null(caption)){
    caption <- command(
      'caption',
      options=cap,
      args=paste(caption,label)
    )
    x <- c(
      if(cap.top)caption else NULL,
      x,
      if(!cap.top)caption else NULL
    )
  }
  x <- wrap(
    x,
    'table',
    options=options
  )
  class(x) <- c('ltable',class(x))
  if(is.null(file))return(x)
  else {
    writeLines(x,file)
    invisible(x)
  }
}	

as.tabular <- function(x,...)UseMethod('tabular')
tabular <- function(x,...)UseMethod('tabular')
tabular.data.frame <- function(
  x,
  rules=c(2,1,1),
  walls=0,
  grid=FALSE,
  rowgroups=factor(rownames(x)),
  colgroups=factor(names(x)),
  rowbreaks=if(grid)breaks(rowgroups,...)else 0,
  colbreaks=if(grid)breaks(colgroups,...)else 0,
  rowgrouprule = 0,
  colgrouprule = 0,
  rowcolors=NULL,
  rowgrouplabel=' ',
  charjust='left',
  numjust='right',
  justify=ifelse(sapply(x,is.numeric),numjust,charjust),
  colwidth=NA,
  paralign='top',
  na='',
  verbatim=ifelse(sapply(x,is.numeric),TRUE,FALSE),
  escape='#',
  trim=TRUE,
  source=NULL,
  file=NULL,
  source.label='source: ',
  file.label='file: ',
  basefile=FALSE,
  tabularEnvironment='tabular',
  footnote.size = 'tiny',
  ...
){
  #groom arguments 
  # shall there be row group labels and column group labels?  
  groupcols <- inherits(colgroups, 'character')
  grouprows <- inherits(rowgroups, 'character')
  stopifnot(length(rowgrouprule) == 1, length(colgrouprule) == 1)
  x <- as.data.frame(x)
  rules <- rep(rules, length.out = 3)
  walls <- rep(walls, length.out = 2)	  
  rowgroups <- rep(rowgroups, length.out=nrow(x))
  colgroups <- rep(colgroups, length.out=ncol(x))
  rowbreaks <- rep(rowbreaks, length.out=nrow(x)-1)
  colbreaks <- rep(colbreaks, length.out=ncol(x)-1)
  if(!is.null(rowcolors))rowcolors <- rep(rowcolors, length.out=nrow(x))
  stopifnot(length(charjust)==1)
  stopifnot(length(numjust)==1)	
  stopifnot(length(escape)==1)	
  stopifnot(charjust %in% c('left','right','center'))
  stopifnot(numjust %in% c('left','right','center'))
  
  # if grouprows, rowgroups becomes a column, affecting following ncol
  if(grouprows){
   x[rowgrouplabel] <- rowgroups
   x <- shuffle(x,rowgrouplabel) # place first
  }
  partial <- glue('\\cline{',2,'-',ncol(x),'}')
  multirow <- function(x){
    node <- runhead(x)
    blocks <- cumsum(node)
    extent <- reapply(blocks,blocks, length)
    y <- glue('\\multirow{',extent,'}{*}{',x,'}')
    y[!node] <- ''
    y
  }
  if(grouprows) x[,1] <- multirow(x[,1])
  mitigate <- function(arg,cols,rowgroupval){
    if(length(arg) == 1) arg <- rep(arg, length.out=cols)
    if(length(arg) == cols - 1) arg <- append(arg,rowgroupval,0)
    stopifnot(length(arg) == cols)
    arg
  }
  colbreaks <- mitigate(colbreaks, ncol(x) - 1, rowgrouprule)
  na <- mitigate(na, ncol(x), '')
  verbatim <- as.logical(mitigate(verbatim,ncol(x),FALSE))
  paralign <- map(paralign,from=c('top','middle','bottom'),to=c('p','m','b'))[[1]]
  colwidth <- mitigate(colwidth,ncol(x), NA)
  colwidth <- sub('^',glue(paralign,'{'),colwidth)
  colwidth <- sub('$','}',colwidth)
  justify <- mitigate(justify, ncol(x), 'left')
  decimal <- justify=='decimal'
  justify <- map(justify, from=c('left','right','center','decimal'),to=c('l','r','c','r'))
  justify[!is.na(colwidth)] <- colwidth[!is.na(colwidth)]
  format <- tabularformat(justify=justify, breaks=colbreaks, walls=walls) #ready
  header <- row2tabular(names(x)) #ready
  if(grouprows & groupcols) colgroups <- c('',colgroups)
  multicol <- function(x,colbreaks,justify,walls){
    node <- runhead(x)
    blocks <- cumsum(node)
    extent <- reapply(blocks,blocks, length)
    rightbreaks <- c(colbreaks,walls[[2]])
    leftbreaks <- c(walls[[1]],rep('0',length(justify) - 1))
    rightbreaks <- sapply(rightbreaks,function(b)paste(collapse='',rep('|',as.numeric(b))))
    leftbreaks  <- sapply(leftbreaks,function(b)paste(collapse='',rep('|',as.numeric(b))))
    battery <- data.frame(
      stringsAsFactors=FALSE,
      blocks=blocks,
      extent=extent,
      leftbreaks=leftbreaks,
      justify=justify,
      rightbreaks=rightbreaks,
      x=x
    )
    # the relevant break on any multicol is that associated with the last member of the block
    battery$justify <- 'c'
    battery$rightbreaks <- last(battery$rightbreaks,within=battery$blocks)
    y <- with(battery, glue('\\multicolumn{',extent,'}{',leftbreaks,justify,rightbreaks,'}{',x,'}'))
    y[!node] <- ''
    y
  }
  if(groupcols) colgroups <- multicol(colgroups,colbreaks,justify,walls)
  header2 <- row2tabular(colgroups[colgroups!=''])
 
  sapply(names(x)[verbatim],function(nm)if(any(!is.na(x[[nm]]) & contains(escape,x[[nm]],fixed=TRUE)))warning(nm,'contains', escape))
  x[] <- lapply(seq_along(x),function(col)if(decimal[[col]])align.decimal(x[[col]],...)else format(x[[col]],trim=trim,...))
  x[] <- lapply(seq_along(x),function(col)sub('^ *NA *$',na[[col]],x[[col]]))
  x[] <- lapply(seq_along(x),function(col)if(verbatim[[col]])glue('\\verb',escape,x[[col]],escape)else x[[col]])
  x <- as.matrix(x)
  x <- apply(x,1,row2tabular) #ready
  # we now have a format string, a header, and all rows as character vector
  # splice in the horizontal rules
  # we treat header as a row, and treat rules[2:3] as pertaining to first and last row.
  # we create an empty row to represent pre-header
  if(!is.null(rowcolors))x <- glue('\\rowcolor{',rowcolors,'} ',x)
  x <- c('',if(groupcols) header2, header,x)
  oldbreaks <- rowbreaks
  rowbreaks <- c(rules[[1]],if(groupcols) colgrouprule,rules[[2]], rowbreaks,rules[[3]])
  stopifnot(length(rowbreaks)==length(x))
  # the line end style depends on position in a rowgroup block.  Only end-of-block
  # may have a full line.
  full <- '\\hline'
  rowgroupstyle <- rev(runhead(rev(rowgroups)))
  # last of these is irrelevant, as 'rules' controls final aesthetic
  rowgroupstyle <- rowgroupstyle[-length(rowgroupstyle)]
  rowgroupstyle <- ifelse(rowgroupstyle,full,partial)
  
  style <- c(
    full, # top
    if(groupcols) full, # below grouping labels
    full, # below regular labels
    rowgroupstyle, # regular data
    full # bottom
  )  
  while(any(rowbreaks > 0)){
    x[rowbreaks > 0] <- paste(x[rowbreaks > 0],style[rowbreaks > 0])
    rowbreaks <- rowbreaks - 1
  }
  x <- wrap(x,tabularEnvironment,args=format)
  class(x) <- c('tabular',class(x))
    if(!is.null(source))if(!is.null(source.label)) x <- c(x,glue('\\\\{\\',footnote.size,' ',source.label,source,'}'))
  if(!is.null(file))if(!is.null(file.label)) x <- c(
    x,
    glue(
      '\\\\{\\',footnote.size,' ',
      file.label,
      if(basefile)basename(file) else file,
      '}'
    )
  )
  if(is.null(file))return(x)
  else{
      writeLines(x,file)
      invisible(x)
  }
}

tabular.table <- function(x, ...){
  if(length(dim(x)) != 2) stop('tabular.table only implemented for 2-dimensional tables')
  class(x) <- 'matrix'
  tabular(x, ...)
}
ltable.table <- function(x, ...){
  if(length(dim(x)) != 2) stop('ltable.table only implemented for 2-dimensional tables')
  class(x) <- 'matrix'
  ltable(x, ...)
}
tabular.matrix <- function(x,...){
  y <- as.data.frame(x)
  dimnames <- dimnames(x)
  if(!is.null(dimnames)){
    nms <- names(dimnames)
    rows <- nms[[1]]
    if(!is.na(rows)){
      y[,rows] <- rownames(x)
      y <- shuffle(y, rows) # move to front
    }
  }
  tabular(y,...)
}
ltable.matrix <- function(x, caption = names(dimnames(x))[[2]],...){
  y <- as.data.frame(x)
  dimnames <- dimnames(x)
  if(!is.null(dimnames)){
    nms <- names(dimnames)
    rows <- nms[[1]]
    if(!is.na(rows)){
      y[,rows] <- rownames(x)
      y <- shuffle(y, rows) # move to front
    }
  }
  ltable(y,caption=caption, ...)
}
  

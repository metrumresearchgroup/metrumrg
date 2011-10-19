lhs <- function(x,...)sub(' *~.*','',x,...)
rhs <- function(x,...)sub('.*~ *','',x,...)
nospace <- function(x,...)gsub(' ','',x)
tos <- function(x,...){
  at <- regexpr('(theta|omega|sigma)_[0-9.]+',x,ignore.case=TRUE)
  length <- attr(at,'match.length')
  x <- substr(x,start=at, stop=at+length-1)
  x <- sub('_','',x)
  x <- toupper(x)
  x
}
closers <- function(x,sub,sup,...){
  stack <- character(0)
  for(i in seq_along(x)){
    this <- x[[i]]
    if(this=='_')stack <- append(stack,'sub')
    if(this=='^')stack <- append(stack,'sup')
    if(this==' ' & length(stack)){
      current <- stack[[length(stack)]]
      stack <- stack[-length(stack)]
      x[[i]] <- if(current=='sub') sub else sup
    }
  }
  #close out dangling nests
  while (length(stack)){
      current <- stack[[length(stack)]]
      stack <- stack[-length(stack)]
      x <- append(x,if(current=='sub') sub else sup)
  }
  x
}
wiki2latex    <- function(x,...)wikiparse(x,sim='\\\\sim',dot='\\\\cdot',pregreek = '\\\\',wrap=c('$\\mathrm{','}$'),...)
wiki2plotmath <- function(x,...)wikiparse(x,sim='%~~%',dot='%.%',sub='',openSub='[',closeSub=']',...)
wiki2label    <- function(x,...)nospace(noUnits(lhs(x)))
wiki2parameter<- function(x,...)tos(x)
wikiparse <- function(
  x,
  sim='~',
  dot='*',
  pregreek='',
  sup='^',
  openSup='{',
  closeSup='}',
  sub='_',
  openSub='{',
  closeSub='}',
  wrap='',
  ...
){
  x <- sub('^ *','',x) #strip leading white
  x <- sub('~',sim,x) #substitute equality symbol
  x <- gsub('\\*',dot,x) #substitute multiply symbol
  x <- gsub('(theta|omega|sigma|eta)',glue(pregreek,'\\1'),x,ignore.case=TRUE) #prefix greek symbols
  #replace sub/sup closers
  x <- sapply(x,strsplit,split='')
  x <- lapply(x,closers, sub=closeSub,sup=closeSup)
  x <- sapply(x,paste,collapse='')
  #replace sup/sub openers
  x <- gsub('_',glue(sub,openSub),x)
  x <- gsub('\\^',glue(sup,openSup),x)
  x <- glue(wrap[[1]],x,wrap[[length(wrap)]])
  x
}
justUnits <- function(x,...){
  at <- regexpr('\\([^)]*\\)',x,ignore.case=TRUE)
  length <- attr(at,'match.length')
  x <- substr(x,start=at+1, stop=at+length-2)
  x
}
noUnits <- function(x,...)sub('\\([^)]*\\)','',x)

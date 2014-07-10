getwd()
library(metrumrg)
as.define <- function(x, ...)UseMethod('as.define')

toSAS.timepoint <- function(x,...)as.character(x)
as.define.spec <- function(x,sep = ' = ',collapse = '; ',escape = c('_','%','$'),...){
  x$required <- NULL
  names(x) <- c('Variable', 'Label', 'Type', 'Codes','Comments')
  x$Type <- map(x$Type, from = c('character','numeric','integer','datetime'),
                to = c('char','num','num','char'))
  codes <- codes(x$Codes)
  decodes <- decodes(x$Codes)
  blend <- function(codes,decodes){
    if(length(codes) == 1) if(is.na(codes))return(as.character(NA))
    stopifnot(length(codes) == length(decodes))
    list <- paste(codes,decodes,sep = sep)
    list[is.na(decodes)] <- codes[is.na(decodes)]
    string <- paste(list,collapse = collapse)
    string
  }
  x$Codes <- sapply(seq_along(codes),function(i)blend(codes[[i]],decodes[[i]]))
  polish <- function(x,escape){
    for(i in escape)x <- gsub(i,glue('\\',i),x,fixed = TRUE)
    x
  }    
  x[] <- lapply(x, polish,escape = escape)
  class(x) <- c('define','keyed','data.frame')
  x
}

as.pdf.define <- function(
  x,
  stem,
  caption = '',
  grid = TRUE,
  rules = 1,
  colwidth = c('1in','1in','0.5in','1.5in','1.5in'),
  morePreamble = command('usepackage',args = 'longtable'),
  tabularEnvironment = 'longtable',
  walls = 1,
  geoLeft = '1in',
  geoRight = '1in',
  geoTop = '1in',
  geoBottom = '1in',
  tabnum = FALSE,
  pretable = paste(if(tabnum) '\\caption{' else '\\caption*{',caption,'}\\\\'),
  prepos = 1,
  headerBold = TRUE,
  pagestyle = command("pagestyle", args = "plain"),
  ...
){
  if(headerBold) names(x) <- glue('\\textbf{',names(x),'}')
  tab <- tabular(
    x,
    grid = grid,
    rules = rules,
    colwidth = colwidth,
    tabularEnvironment = tabularEnvironment,
    walls = walls,
    ...
  )
  tab <- append(tab,pretable,prepos)
  tex <- as.document(
    morePreamble = morePreamble,
    geoLeft = geoLeft,
    geoRight = geoRight,
    geoTop = geoTop,
    geoBottom = geoBottom,
    pagestyle = pagestyle,
    tab,
    ...
  )
  as.pdf(stem=stem,tex,...)
}

x <- read.spec('data/derived/tranPKPD1.spec')
y <- as.define(x)
head(y$Comments)
y$Comments <- gsub(',',', ',y$Comments)
as.pdf(y,stem = 'define',caption = 'Data Items (tran.xpt)')
system('open define.pdf')


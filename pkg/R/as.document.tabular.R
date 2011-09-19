as.document <- function(x,...)UseMethod('as.document')
as.document.tabular <- function(
  x,
  rules = c(2, 1, 1), 
  walls = 0, 
  grid = FALSE, 
  rowgroups = rownames(x), 
  colgroups = names(x), 
  rowbreaks = if (grid) breaks(rowgroups, ...) else 0,
  colbreaks = if (grid) breaks(colgroups, ...) else 0, 
  charjust = "left", 
  numjust = "right", 
  justify = ifelse(sapply(x, is.numeric), numjust, charjust), 
  colwidth = NA, 
  paralign = "top", 
  na = "", 
  verbatim = ifelse(sapply(x, is.numeric), TRUE, FALSE), 
  escape = "#", 
  trim = TRUE, 
  wider=0,
  longer=0,
  prolog=NULL,
  epilog=NULL,
  ...
){
  stopifnot(inherits(x,'data.frame'))
  rules <- rep(rules, length.out = 3)
  walls <- rep(walls, length.out = 2)
  rowbreaks <- rep(rowbreaks, length.out = nrow(x) - 1)
  colbreaks <- rep(colbreaks, length.out = ncol(x) - 1)
  text <- maxChar(do.call(paste,fixedwidth(x)))
  bars <- c(walls,colbreaks)
  bars <- sum(bars) + sum(bars[bars>1]-1)*4 
  #bars[bars>1] - 1 gives the number of inter-bar gaps, which are about 4 times as wide as a bar.
  #same logic applies to lines.
  
  wide <- text * 2.36 + bars*0.14 + 5.9 + wider
  
  rows <- 1+nrow(x)
  lines <- c(rules,rowbreaks)
  lines <- sum(lines) + sum(lines[lines>1]-1)*4
  
  long <- rows*4.21 + lines*0.16 + 2 + longer
  
  papersize <- glue('{',wide,'mm',',',long,'mm}')
  doc <-  c(
    command('documentclass',args='article'),
    command(
      'usepackage',
      options=list(
          left='1mm',
          top='1mm',
          bottom='1mm',
          right='1mm'
      ),
      args='geometry'
    ),
    command('geometry',args=list(glue('papersize=',papersize))),
    wrap(
      environment='document',
      c(
      	command('thispagestyle',args='empty'),
     	command('pagestyle',args='empty'),
        prolog,
        tabular(
          x=x,
          rules=rules,
          walls=walls,
          grid=grid,
          rowgroups=rowgroups,
          colgroups=colgroups,
          rowbreaks=rowbreaks,
          colbreaks=colbreaks,
          charjust=charjust,
          numjust=numjust,
          justify=justify,
          colwidth=colwidth,
          paralign=paralign,
          na=na,
          verbatim=verbatim,
          escape=escape,
          trim=trim,
          ...
        ),
        epilog
      )
    )
  )
  class(doc) <- c('document',class(doc))
  doc
}
as.pdf <- function(x,...)UseMethod('as.pdf')
as.pdf.document <- function(x,stem,clean=TRUE,...){
	tex <- glue(stem,'.tex')
	writeLines(x,tex)
	cmd <- paste('pdflatex',tex)
	result <- system(cmd)
	possibles <- glue(stem,c('.tex','.log','.aux','.out'))
	actuals <- possibles[file.exists(possibles)]
	if(clean)file.remove(actuals)
	invisible(result)
}
as.pdf.tabular <- function(x,...)as.pdf(as.document(x,...),...)
as.pdf.data.frame <- function(x,...)as.pdf(tabular(x,...),...)
as.document <- function(x,...)UseMethod('as.document')
as.document.tabular <- function(x,wide,long,prolog=NULL,epilog=NULL,...){
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
		x,
		epilog
	      )
	    )
	  )
	  class(doc) <- c('document',class(doc))
	  doc
}
as.document.data.frame <- function(
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
  
  tab <- tabular(
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
  )
  doc <-  as.document(tab,wide,long,prolog=prolog,epilog=epilog,...)
  doc
}
as.pdf <- function(x,...)UseMethod('as.pdf')
as.pdf.document <- function(x,stem,dir='.',clean=TRUE,...){
	outfile <- glue(stem,'.tex')
	outpath <- file.path(dir,outfile)
	writeLines(x,outpath)
	cmd <- glue('pdflatex -output-directory=',dir,' ',outpath)
	result <- system(cmd)
	possibles <- glue(stem,c('.tex','.log','.aux','.out'))
	actuals <- possibles[file.exists(possibles)]
	actualpaths <- file.path(dir,actuals)
	if(clean)file.remove(actualpaths)
	invisible(result)
}
as.pdf.tabular <- function(x,wide,long,stem,...)as.pdf(as.document(x,wide=wide,long=long,...),stem=stem,...)
as.pdf.data.frame <- function(x,stem,...)as.pdf(as.document(x,...),stem=stem,...)
as.document <- function(x,...)UseMethod('as.document')
as.document.character <- function(
	x,
	landscape=FALSE,
	wide=if(landscape) 279.4 else 215.9,
	long=if(landscape) 215.9 else 279.4,
	prolog=NULL,
	epilog=NULL,
	...
){
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
as.pdf.document <- function(
	x,
	stem,
	dir='.',
	clean=TRUE,
	...
){
	if(missing(stem))stop('a file stem (no extension) must be provided')
	if (contains('\\.pdf$',stem,ignore.case=TRUE)){
		warning('stripping .pdf from file stem ...')
		stem <- sub('\\.pdf$','',stem,ignore.case=TRUE)
	}
	outfile <- glue(stem,'.tex')
	outpath <- file.path(dir,outfile)
	writeLines(x,outpath)
	cmd <- glue('pdflatex -output-directory=',dir,' ',outpath)
	result <- system(cmd)
	variants <- glue(stem,c('.tex','.log','.aux','.out'))
	possibles <- file.path(dir,variants)
	actuals <- possibles[file.exists(possibles)]
	if(clean)file.remove(actuals)
	invisible(result)
}
as.pdf.character <- function(
	x,
	stem,
	landscape=FALSE,
	wide=if(landscape) 279.4 else 215.9,
	long=if(landscape) 215.9 else 279.4,
	...
)as.pdf(
	as.document(
		x,
		wide=wide,
		long=long,
		...
	),
	stem=stem,
	...
)
as.pdf.data.frame <- function(x,stem,...)as.pdf(as.document(x,...),stem=stem,...)

as.pdf.tex <- function(
	x,
	stem=NULL,
	dir=NULL,
	landscape=FALSE,
	clean=TRUE,
	onefile=FALSE,
	...
){
	stopifnot(
		length(x)>0,
		all(file.exists(x)),
		length(stem)==1 | length(stem)==length(x) | stem==NULL,
		length(stem)==1 | onefile==FALSE | stem==NULL,
		length(dir)==1 | length(dir)==length(x) | dir==NULL
	)
	is.tex <- sapply(x,function(nm)contains('\\.tex',nm,ignore.case=TRUE))
	if(any(!is.tex))warning('x is expected to be a vector of tex file names')
	dat <- lapply(x,readLines)
	if(is.null(stem))stem <- sub('\\.[^.]+$','',basename(x),ignore.case=TRUE)
	if(is.null(dir))dir <- dirname(x)
	dir <- rep(dir,length.out=length(stem))
	if(onefile)stem <- stem[[1]]
	if(onefile)dir <- dir[[1]]
	if(onefile)dat <- list(unlist(dat))
	stopifnot(length(dat)==length(stem),length(dat)==length(dir))
	target <- glue(stem,'_doc')
	for(index in seq_along(dat)){
		as.pdf.character(
			dat[[index]],
			stem=target[[index]],
			dir=dir[[index]],
			landscape=landscape,
			clean=clean,
			...
		)
	}
	invisible(file.path(dir,glue(target,'.pdf')))
}

viewtex <- function(x,delete=TRUE,latency=1,...){
	newfiles <- as.pdf.tex(x,...)
	sapply(newfiles,browseURL)
	if(delete)Sys.sleep(latency)
	if(delete)sapply(newfiles,unlink)
	invisible(newfiles)
}

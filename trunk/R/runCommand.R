`runCommand` <-
  function (
  	command,
  	...,
	run,
	rdir,
	boot,
	urgent,
	checksum,
	grid,
	udef=FALSE,
	ctlfile,
	outfile,
	perl=if(nix())'perl -S'else if(!invisible) 'cmd /K perl -S' else 'cmd /C perl -S' ,
	intern=invisible,
	minimized=invisible,
	invisible=FALSE,
	split=grid,
	N=glue('Run',run,if(split)c('c','e') else NULL),
	o=rdir,
	e=rdir,
	L=if(split & interface=='nm.pl')c(compileflag(compiler(config(dirname(command)))),NA)else NA,
	hold_jid=if(split)c(NA,glue('Run',run,'c'))else NA,
	V='',
	j='y',
	q=if(split)
	    c(
	        'compile.q',
	        if(urgent)'all.q' else 'bootstrap.q'
	    )
	else 
	    if(!execute)
	        'compile.q'
            else if(urgent)'all.q' else 'bootstrap.q',
	sync=if(boot)'n'else'y',
	shell='n',
	b='y',
	cwd='',
	compile=TRUE,
	execute=TRUE,
	background=FALSE,
	interface = 'nm.pl'
){
  force(L) #before command changes
  if(nix())internal <- FALSE

  #draft a command
  if(!udef)command <- match.fun(interface)(
  	command=command,
  	run=run,
  	rdir=rdir,
  	boot=boot,
  	urgent=urgent,
  	checksum=checksum,
  	grid=grid,
  	ctlfile=ctlfile,
  	outfile=outfile,
  	perl=perl,
  	split=split,
  	compile=compile,
  	execute=execute,
  	...
  )
  if(grid) command <- qsub(command,N=N,o=o,e=e,l=L,hold_jid=hold_jid,V=V,j=j,q=q,sync=sync,shell=shell,b=b,cwd=cwd,...)
  if(background) command <- paste(command,'&')
  

  #set up the call
  execute <- function(command,intern,minimized,invisible,win){
	args <- list(command, intern=intern)
        if (win()) args <- c(args,list(minimized=minimized, invisible=invisible))
        do.call(system,args)
  }
  lapply(command,execute,intern=intern,minimized=minimized,invisible=invisible,win=win)
}


qsub <- function(
	command,
	...
){
	range <- c(
		'@','a','ac','A','b','c','ckpt','clear','cwd','C',
		'dc','dl','e','hard','h','help','hold_jid','i','j','js',
		'l','m','M','masterq','notify','now','N','o','P','p',
		'pe','q','R','r','sc','shell','soft','sync','S','t',
		'terse','u','v','verbose','verify','V','w','wd'
	)
	args <- list(...)
	args <- args[names(args) %in% range]
	if(length(args))names(args) <- glue('-',names(args))	
	vectors <- c(as.list(names(args)),args)
	vectors <- vectors[t(matrix(seq(length.out=length(vectors)),ncol=2))]
	string <- do.call(paste,vectors)
	result <- paste('qsub',string,command)
	result <- gsub('-[^ ]* NA','',result)
	result
}
config <- function(dir,...)file.path(dir,'config.xml')
compiler <- function(config,pathsep='/',...){
	tree <- xmlParse(config)
	nmtran <- xmlValue(getNodeSet(tree,"//d:instruction[@id='nmtran']/text()",c(d='http://metruminstitute.org/nmqual/configuration'))[[1]])
	nmtran <- sub('^ *','',nmtran)
	comp <- strsplit(nmtran,' ')[[1]]
	rev(strsplit(comp,pathsep)[[1]])[[1]]
}
nmVersion <- function(config,...){
	tree <- xmlParse(config)
	as.numeric(getNodeSet(tree,"//d:nonmem/@version",c(d='http://metruminstitute.org/nmqual/configuration'))[[1]])
}
nm.pl <- function(
	command,
	ctlfile,
	outfile=NULL,
	perl='perl',
	checksum=TRUE,
	compile=TRUE, 
	execute=TRUE,
	split=FALSE,
	...
){
	if(split & xor(compile,execute)) stop('cannot split run if compile or execute is FALSE')
	if(is.null(outfile))outfile <- sub('\\....$','.lst',ctlfile)
	command <- paste(perl,command)
	stage <- c('c','e')[c(compile,execute)][xor(compile,execute)|split]
	if(length(stage))command <- paste(command,stage)
	command <- paste(command,ctlfile,outfile)
	if(!checksum) command <- paste(command,'nochecksum')
	command
}
autolog.pl <- function(
	command,
	rdir,
	run,
	perl='perl',
	compile=TRUE, 
	execute=TRUE,
	split=FALSE,
	config = file.path(dirname(command),'log.xml'),
	mode='run',
	...
){
	if(split & xor(compile,execute)) stop('cannot split run if compile or execute is FALSE')
	stage <- if(split)c('c','e')else'ce'
	if(compile==FALSE)stage <- 'e'
	if(execute==FALSE)stage <- 'c'
	command <- paste(perl,command,config,mode,stage,rdir,run)
	command
}

compileflag <- function(compiler,mappings=list(ifort=1),...){
	val <- mappings[[compiler]]
	if(is.null(val)) NA else paste(sep='=','compile',val)
}











































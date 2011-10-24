`runNonmem` <-
function (
	run,
	command,
	project,
	boot,
	urgent,
	checkrunno,
	diag,
	fdata,
	epilog,
	dvname,
	logtrans,
	grp,
	grpnames,
	cont.cov,
	cat.cov,
	par.list,
	eta.list,
	missing,
	invisible,
	checksum,
	grid,
	nice,
	udef,
	compile,
	execute,
	split,
	plotfile=plotfilename(run,project,grp),
	runext = if(boot) '.boot' else if(grid) '.lock' else '',
	rundir = filename(project,run,runext),
	outfile = filename(rundir,run,'.lst'),
	streams = project,
	ctlfile = filename(streams,run,'.ctl'),
	remove = c(
		"^F[ISRCMP]","^OU","^nonmem", "^nul$",
		"WK","LNK$","fort","^nm","lnk$","set$",
		"^gar","INT","^temp","^tr","^new",
		if(fdata)c('^FD','^PR')
	),
	sync=if(boot)'n'else'y',
	interface='nm.pl',
	...,
	perm.cond=NULL
){
  # Note: runNonmem calls runCommand, which supports the qsub argument 'pe';
  # it also calls PLOTR, which supports the trellis.skeleton argument 'perm.cond'.
  # If 'pe' is passed to runNonmem, trellis.skeleton partial-matches it as 'perm.cond'.
  # To disambiguate, runNonmem should declare at least one of them, and pass explicitly.
  # Currently we declare 'perm.cond' and pass only to PLOTR. Thus, trellis.skeleton
  # always receives a fully-named 'perm.cond' (with the usual default value) 
  # and will do no partial matching.  
  
  #Define some functions.
  final <- function(x)sub('\\.lock','',x)
 
  #Groom arguments.
  rundir <- star(rundir,run)
  ctlfile <- star(ctlfile,run)
  outfile <- star(outfile,run)
  if(!file.exists(ctlfile))stop(ctlfile,' not found')
  control <- read.nmctl(ctlfile)
  outputdomain <- names(control) =='table' | contains('est',names(control))
  control[outputdomain] <- lapply(control[outputdomain],explicitPath)
  if (checkrunno) {
  	  problemdomain <- contains('prob',names(control))
  	  control[problemdomain] <- lapply(control[problemdomain],fixProblem,run=run)
  	  control[outputdomain] <- lapply(control[outputdomain],fixFile,run=run)
  	  write.nmctl(control,file=ctlfile)
  }
  tabfile <- ''
  parfile <- ''
  msffile <- ''
  control <- as.character(control[outputdomain])
  tryCatch(tabfile <- tabfile(control,dir=final(rundir),...),error=function(e)warning('cannot locate *.tab in control stream for run ',run,call.=FALSE,immediate.=TRUE))
  tryCatch(parfile <- parfile(control,dir=final(rundir),...),error=function(e)warning('cannot locate *par.tab in control stream for run ', run,call.=FALSE,immediate.=TRUE))
  tryCatch(msffile <- msffile(control,dir=final(rundir),...),error=function(e)warning('cannot locate *.msf in control stream for run ',run,call.=FALSE,immediate.=TRUE))
  #tabfile <- tabfile(control,dir=final(rundir),...)
  #parfile <- parfile(control,dir=final(rundir),...)
  #msffile <- msffile(control,dir=final(rundir),...)
  script <- NULL
  epimatch <- try(match.fun(epilog),silent=TRUE)
  if(is.function(epimatch))epilog <- epimatch
  else if (class(epilog)=='character'){
	  script <- epilog
	  epilog <- episcript
  }
  
  #Prepare the file environment.
  if(command!='')if(compile){
  	  if(file.exists(plotfile))file.remove(plotfile)
	  if(file.exists(outfile))file.remove(outfile)
	  if(file.exists(tabfile))file.remove(tabfile)
	  if(file.exists(parfile))file.remove(parfile)
	  if(file.exists(msffile))file.remove(msffile)
	  purge.dir(final(rundir),nice)
	  if(rundir!=final(rundir))purge.dir(rundir) #deliberately not "nice"
	  dir.create(rundir, showWarnings = FALSE)
	  dname <- getdname(ctlfile)
	  #The next error trap is redundant: prevents identical trap in getCovs()
	  if(!file.exists(resolve(dname,rundir)))stop(dname,' not visible from ',rundir,call.=FALSE)
	  file.copy(ctlfile, file.path(rundir,basename(ctlfile)), overwrite = TRUE)
  }
  #Run NONMEM.
  if(command=='')message('skipping command')
  else runCommand(
  	command=command,
	run=run,
	rdir=rundir,
	boot=boot,
	urgent=urgent,
	checksum=checksum,
	grid=grid,
	udef=udef,
	ctlfile=file.path(rundir,basename(ctlfile)),
	outfile=outfile,
	invisible=invisible,
	compile=compile,
	execute=execute,
	split=split,
	sync=sync,
	interface=interface,
	...
  )
  #Clean up.
  if(execute){
	  if(sync=='n')return() #because we may have reached here before run is complete.
	  lapply(remove,purge.files,dir=rundir)
	  if(rundir!=final(rundir)){
		dir.create(final(rundir), showWarnings = FALSE)
		file.copy(from=dir(rundir,full.names=TRUE),to=final(rundir),overwrite=TRUE)
		purge.dir(rundir)
	  }
	
	  #Diagnostics
	  if(!udef)
	   if(command!='' & interface=='nm.pl')if(nmVersion(config(dirname(command))) < 7)
	    tryCatch(
    		setCwres(
    			cwres=getCwres(
    				directory=final(rundir)
    			),
    			file=tabfile
    		),
    		error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
    	    )
	  if(diag)tryCatch(
		PLOTR(
			run=run,
			project=project,
			dvname=dvname,
			logtrans=logtrans,
			grp=grp,
			grpnames=grpnames,
			cont.cov=cont.cov,
			cat.cov=cat.cov,
			par.list=par.list,
			eta.list=eta.list,
			missing=missing,
			ctlfile=ctlfile,
			outfile=final(outfile),
			rundir=final(rundir),
			plotfile=plotfile,
			perm.cond=perm.cond,
			...
		),
		error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
	  )
	  if (!is.null(epilog))if(is.function(epilog))tryCatch(
		  epilog(
			run=run,
			project=project,
			dvname=dvname,
			logtrans=logtrans,
			grp=grp,
			grpames=grpnames,
			cont.cov=cont.cov,
			cat.cov=cat.cov,
			par.list=par.list,
			eta.list=eta.list,
			missing=missing,
			ctlfile=ctlfile,
			outfile=final(outfile),
			rundir=final(rundir),
			perm.cond=perm.cond,
			...,
			script=script
		),
		error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
	  )
	  message("Run ", run, " complete.")
  }
}

#.............................................................................
  purge.dir <- function(dir,nice=FALSE){
  	if(file_test('-d',dir)){
  		files <- dir(dir,full.names=TRUE,all.files=!nice)
  		files <- files[!files %in% grep('\\.$',files,value=TRUE)]
  		isDir <- file_test('-d',files)
  		if(length(files[!isDir]))file.remove(files[!isDir])
  		lapply(files[isDir],purge.dir,nice=nice)
  		if(!nice)unlink(dir, recursive=TRUE)
  	}
  }
  purge.files <- function(pattern,dir='.'){
  	if(file_test('-d',dir)){
  		files <- dir(dir)
  		files <- grep(pattern,files,value=TRUE,ignore.case=TRUE)
  		#if(length(files))file.remove(paste(dir,files,sep='/'))
  		if(length(files)) files <- paste(dir,files,sep='/')
  		lapply(files[file_test('-f',files)],file.remove)
  		lapply(files[file_test('-d',files)],purge.dir)     
  	}
  }
  episcript <- function(script,...){
	 extras <- list(...)
	 args <- names(extras)
	 lapply(
	 	args,
		function(x,extras)assign(x,extras[[x]],envir=parent.frame(2)),
		extras
	)
	try(source(script,local=TRUE))
  }
  fixProblem <- function(x,run)sub('(^ *(RUN#? *)?)([^ ]+)(.*$)',glue('\\1',run,'\\4'),x,ignore.case=TRUE)
  fixFile <- function(x,run){
        x <- explicitPath(x)
	risk <- grep('\\bTAB\\b|\\bMSF\\b',x,ignore.case=TRUE)
        #except <- grep('\\bMSFI\\b',x,ignore.case=TRUE)
        #risk <- setdiff(risk,except)
        dir <- dirname(x)
	base <- basename(x)
	base <- sub('^[^.(par)]+',run,base)
	x[risk] <- file.path(dir[risk],base[risk])
	x
  }
  explicitPath <- function(x){
	risk <- grep('\\.TAB\\b|\\.MSF\\b',x,ignore.case=TRUE)
    	except <- grep('/',x)
    	risk <- setdiff(risk,except)
    	x[risk] <- sub('^(.*\\W)?(\\w*)(\\.msf|\\.tab)(.*)$','\\1./\\2\\3\\4',x[risk],ignore.case=TRUE)
	x
  }
  extractPath <- function(x)sub('(^.*(MSFO?|FILE) *= *)([^ ]*)(.*$)','\\3',x,ignore.case=TRUE)
  resolve <- function(file,dir)ifelse(contains('^\\.',file),file.path(dir,file),file)
  scavenge <- function(expr,lines){
	  x <- lines[grep(expr,lines,ignore.case=TRUE, perl=TRUE)]
	  if(!length(x))stop('expression ',expr,' not found',call.=FALSE)
	  x[[1]]
  }
  extfile <- function(ctlfile,dir,extreg,...){
  	  x <- scavenge(extreg,ctlfile)
  	  x <- extractPath(x)
  	  x <- resolve(x,dir)
  	  x
  }
  tabfile <- function(ctlfile,dir,tabreg='(?<!par)\\.tab',...)tryCatch(
  	extfile(ctlfile,dir,extreg=tabreg,...),
  	error=function(e)stop('cannot locate *.tab in control stream for ',dir,call.=FALSE)
  ) 
  parfile <- function(ctlfile,dir,parreg='par\\.tab',...)tryCatch(
  	extfile(ctlfile,dir,extreg=parreg,...),
  	error=function(e)stop('cannot locate *par.tab in control stream for ',dir,call.=FALSE)
  )
  msffile <- function(ctlfile,dir,msfreg='^(?!\\$MSFI).*\\.msf',...)tryCatch(
  	extfile(ctlfile,dir,extreg=msfreg,...),
  	error=function(e)stop('cannot locate *.msf in control stream for ',dir,call.=FALSE)
  )
	  
	  
	  
	  
	  
	  
	  

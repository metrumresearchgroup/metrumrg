`runNonmem` <-
function (
	run,
	command,
	project,
	wait,
	#urgent,
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
	#runext = if(boot) '.boot' else if(grid) '.lock' else '',
	rundir = filename(project,run),
	outfile = filename(rundir,run,'.lst'),
	streams = project,
	ctlfile = filename(streams,run,'.ctl'),
	purge = TRUE,
	sync=if(wait)'y'else'n',
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
  #final <- function(x)sub('\\.lock','',x)
 
  #Groom arguments.
  rundir <- star(rundir,run)
  ctlfile <- star(ctlfile,run)
  outfile <- star(outfile,run)
  catfile <- filename(rundir,run,'.cat')
  pmnfile <- sub('ctl$','pmn',ctlfile) # to support copy of pmn file where present
  pltfile <- filename(streams,'template','.pmn')
  
  #Immediately we need to get the run directory and cat file open, or return an error.
  if(command!='')if(compile){
  	  #purge.dir(final(rundir),nice)
  	  purge.dir(rundir,nice)
	  #if(rundir!=final(rundir))purge.dir(rundir) #deliberately not "nice"
	  if(!file.exists(dirname(rundir)))stop('cannot find ',dirname(rundir))
	  if(!file.exists(rundir))if(!dir.create(rundir))stop('cannot create ',rundir)
  	  cat(date(),file=catfile,sep='\n') #append is FALSE
  }
  
  #Continue
  if(!file.exists(ctlfile)){
  	  msg <- glue(ctlfile,' not found')
  	  cat(msg,file=catfile,append=TRUE,sep='\n')
  	  return(msg)
  }
  control <- read.nmctl(ctlfile)
  #outputdomain <- names(control) =='table' | contains('est',names(control))
  outputdomain <- names(control) %contains% 'tab|est'
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
  tryCatch(tabfile <- tabfile(control,dir=rundir,...),error=function(e)cat('cannot locate *.tab in control stream for run ',file=catfile,append=TRUE,sep='\n'))
  tryCatch(parfile <- parfile(control,dir=rundir,...),error=function(e)cat('cannot locate *par.tab in control stream for run ',file=catfile,append=TRUE,sep='\n'))
  tryCatch(msffile <- msffile(control,dir=rundir,...),error=function(e)cat('cannot locate *.msf in control stream for run ',file=catfile,append=TRUE,sep='\n'))
  #tabfile <- try(tabfile(control,dir=final(rundir),...))
  #parfile <- try(parfile(control,dir=final(rundir),...))
  #msffile <- try(msffile(control,dir=final(rundir),...))
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
	  dname <- getdname(ctlfile)
	  #The next error trap is redundant: prevents identical trap in getCovs()
	  if(!file.exists(resolve(dname,rundir))){
	  	  msg <- glue(dname,' not visible from ',rundir)
	  	  cat(msg,file=catfile,append=TRUE,sep='\n')
	  	  return(msg)
	  }
	  file.copy(ctlfile, file.path(rundir,basename(ctlfile)), overwrite = TRUE)
	  if(file.exists(pmnfile))file.copy(pmnfile,file.path(rundir(basename(pmnfile)),overwrite = TRUE))
	  else if(file.exists(pltfile))file.copy(pltfile,file.path(rundir(basename(pmnfile)),overwrite=TRUE))
  }
  #Run NONMEM.
  if(command=='')res <- ''
  else res <- runCommand(
    	command=command,
    	run=run,
    	rdir=rundir,
    	wait=wait,
    	#urgent=urgent,
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
	  if(sync=='n')return(res) #because we may have reached here before run is complete.
	  if(purge)purgeRunDir(dirs=rundir,debug=!fdata,...)
	  #if(rundir!=final(rundir)){
		#dir.create(final(rundir), showWarnings = FALSE)
		#file.copy(from=dir(rundir,full.names=TRUE),to=final(rundir),overwrite=TRUE)
		#purge.dir(rundir)
		#rundir <- final(rundir)
		#catfile <- final(catfile)
	  #}
	
	  #Diagnostics
	  if(!udef)
	   if(command!='' & interface=='nm.pl')if(nmVersion(config(dirname(command))) < 7)
	    tryCatch(
    		setCwres(
    			cwres=getCwres(
    				directory=rundir
    			),
    			file=tabfile
    		),
    		error=function(e)cat(e$message,file=catfile,append=TRUE,sep='\n')
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
			outfile=outfile,
			rundir=rundir,
			plotfile=plotfile,
			perm.cond=perm.cond,
			...
		),
    		error=function(e)cat(e$message,file=catfile,append=TRUE,sep='\n')
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
			outfile=outfile,
			rundir=rundir,
			perm.cond=perm.cond,
			...,
			script=script
		),
    		error=function(e)cat(e$message,file=catfile,append=TRUE,sep='\n')
	  )
	  message("Run ", run, " complete.")
  }
  return(res)
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
	  
	  
	  
	  
	  
	  
	  

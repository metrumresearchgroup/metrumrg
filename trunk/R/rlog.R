`rlog` <-function(
  	run, 
	project=getwd(), 
	boot=FALSE, 
	append=TRUE,
	tool='nm7',
	file=filename(project,'CombRunLog.csv'),
	rundir = filename(project, run, if(boot) '.boot' else ''),
	nmlog = file.path(rundir,'NonmemRunLog.csv'),
	nmout = filename(rundir,run,'.lst'),
	#pattern=if(boot)c('^F','^nonmem.exe','^P','^O','^Run') else '^FD',
        pattern = c(
        	'^F[ISRCMP]','^OU','^nonmem','^nul$', 
        	'WK', 'LNK$', 'fort', '^nm', 'lnk$', 
        	'set$','^gar', '^temp', '^tr', 
        	'^new', '^FD','^Run\\d+\\.o\\d+$','^prsizes'
        ),
        ...
){
  stopifnot(
  	length(run)==length(unique(run)),
  	length(rundir)==length(run),
  	length(nmlog)==length(run),
  	length(nmout)==length(run)
  )
  testfile <- c('FCON','FILE10','INTER')
  if(tool=='nm6')testfile <- c('FCON','FILE10','OUTPUT')
  state <- sapply(rundir,function(dir,...)runstate(rundir=dir,testfile=testfile,...),...)
  if(!append)if(length(file))if(file.exists(file)) file.remove(file)
  specialize <- function(path,run,nm){
  		if(
  			!length(path) %in% c(0,1,length(run))
  		)stop(
  			'length of' ,nm, ' must be 0, 1, or same as run',call.=FALSE
  		)
  		if(!length(path))return(path)
  		if(length(path)==1) path <- sapply(run,function(x)gsub('*',x,path,fixed=TRUE))
  		names(path) <- run
  		path
  }
  rundir <- specialize(rundir,run,'rundir')
  nmlog <- specialize(nmlog,run,'nmlog')
  nmout <- specialize(nmout,run,'nmout')
  #cleanup
  if(length(pattern)){
  		lapply(
  			rundir[state=='done'],
  			function(dir,pattern)lapply(
  				pattern,
  				purge.files,
  				dir=dir
  			),
  			pattern=pattern
  		)
  }
  unilist <- lapply(
  	seq(length.out=length(run)),
  	function(index,run,nmlog,nmout,tool){
  		res <- tryCatch(
  			as.unilog.run(
  				run=run[[index]],
  				logfile=nmlog[[index]],
  				outfile=nmout[[index]],
  				tool=tool
  			),
  			error=function(e)data.frame(
  				stringsAsFactors=FALSE,
  				tool=tool,
  				run=run[[index]],
  				parameter='min',
  				moment='status',
  				value='-1'
  			)
  		)
  		res
  	},
  	run=run,
  	nmlog=nmlog,
  	nmout=nmout,
  	tool=tool,
  	...
  )
  unilist <- unilist[sapply(unilist,function(r)!is.null(r))]
  if(length(file)){
  	runloglist <- lapply(unilist,as.runlog.unilog)
  	lapply(
  		runloglist,
  		write.table,
  		file=file,
  		append=TRUE,
  		sep=',',
  		row.names=FALSE,
  		col.names=FALSE,
  		quote=FALSE,
  		na='.'
  	)
  }
  uni <- do.call(rbind,unilist)
  invisible(uni)
}	

runstate <- function(
	run,
	project=getwd(),
	rundir=file.path(project,run),
	testfile=c('FCON','FILE10','INTER'),
	queued=   c(0,0,0),
	compiled= c(1,0,0),
	running=  c(1,1,1),
	done=     c(NA,0,1),
	...
){
	#FCON: nm/ABLOCK.f
	#FILE10: nm/BLKDAT.f
	#OUTPUT: nm/BEGIN.f
	if(!missing(run))if(length(run)!=1)stop('run must be scalar',call.=FALSE)
	if(!missing(project))if(length(project)!=1)stop('project must be scalar',call.=FALSE)
	if(!missing(rundir))if(length(rundir)!=1)stop('rundir must be scalar',call.=FALSE)
	stopifnot(
		length(queued)==length(testfile),
		length(compiled)==length(testfile),
		length(running)==length(testfile),
		length(done)==length(testfile)
	)
	#For any given run, NONR supports run/, run.lock/, and run.boot/.
	variants <- glue(rundir,c('.boot','.lock',''))
	variants <- variants[file.exists(variants)]
	if(length(variants))rundir <- variants[[1]] else return('indeterminate')
	testpath=file.path(rundir,testfile)
	state <- file.exists(testpath)
	possible <- rbind(queued,compiled,running,done)
	dimnames(possible)[[2]] <- testfile
	possible <- structure(
		as.logical(possible),
		dim=dim(possible),
		dimnames=dimnames(possible)
	)
	possible <- possible[
		apply(possible,MARGIN=1,FUN=function(x)all(x[!is.na(x)]==state[!is.na(x)])),
		,
		drop=FALSE
	]
	if(nrow(possible)==1) return(rownames(possible))
	else return('indeterminate')
}

progress <- function(run, project=getwd(),...){
	states <- c('queued','compiled','running','done','indeterminate')
	state <- sapply(run,runstate,project=project,...)
	table <- table(state)
	names(table) <- states
	is.na(table) <- 0
	table
}
	
follow <- function(run,project=getwd(),interval=10,watch='done',until=length(run),visible=TRUE,...){
	progress <- progress(run=run,project=project,...)
	if(visible)print(progress)
	if(progress[watch]< until){
		Sys.sleep(interval)
		follow(
			run=run,
			project=project,
			interval=interval,
			watch=watch,
			until=until,
			visible=visible,
			...
		)
	}
	invisible(NULL)
}
	













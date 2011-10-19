`PLOTR` <-function(
	run, 
	project=getwd(), 
	rundir=filename(project,run),
	grp = NULL, 
	onefile=TRUE,
	plotfile=plotfilename(run,project,grp,onefile),
	logtrans = FALSE, 
	dvname = 'DV', 
	epilog=NULL,
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99,
	estimated = NULL,
	...
){
    
    #process data
    data <- dataSynthesis(
    	run=run,
	project=project,
	logtrans=logtrans,
	grp=grp,
	grpnames=grpnames,
	cont.cov=cont.cov,
	cat.cov=cat.cov,
	par.list=par.list,
	eta.list=eta.list,
	missing=missing,
	rundir=rundir,
	...
    )
    write.csv(data,filename(rundir,ext='_syn.csv'),row.names=FALSE)
    available <- names(data)
    cont.cov <- strain(cont.cov,available)
    cat.cov <- strain(cat.cov,available)
    par.list <- strain(par.list,available)
    eta.list <- strain(eta.list,available)
    
    listfilename <- file.path(rundir,glue(run,'.lst'))
    listfile <- readLines(listfilename)
    iterations <- try(iterations(listfile))
    it.dat <- NULL
    if(inherits(iterations,'data.frame'))try(it.dat <- melt(iterations,measure.var=names(iterations)[contains('X',names(iterations))]))
    if(!is.null(estimated)){
    	    if(any(duplicated(estimated)))warning('estimated contains duplicates and will not be used')
    	    else try(levels(it.dat$variable) <- estimated)
    }
    #open device
    plotfile <- star(plotfile,run)
    safe.call(pdf,file=plotfile,onefile=onefile,...)

    #make plots
    lapply(diagnosticPlots(data, dvname=dvname, group='grpnames', model= paste('Model',run),...),print)
    lapply(covariatePlots(data,cont.cov,cat.cov,par.list,eta.list,...),print)
    lapply(cwresPlots(data,cont.cov,cat.cov,...),print)
    if(!is.null(it.dat))try(print(xyplot(value~iteration|variable,it.dat[it.dat$course=='parameter',],main= paste('Model',run,'parameter search'),type='l',ylab='scaled parameter',as.table=TRUE,scales=list(y=list(relation='free')))))
    if(!is.null(it.dat))try(
    	print(
		xyplot(
			value~iteration|variable,
			it.dat[it.dat$course=='gradient',] ,
			main= paste('Model',run,'gradient search'),
			type='l',
			ylab='gradient',
			as.table=TRUE,
			scales=list(y=list(relation='free')),
			panel=function(...){
				panel.abline(h=0)
				panel.xyplot(...)
			}
		)
	)
)

    #cleanup
    dev.off()
    unlink(filename(rundir,ext='_syn.csv'))
    message('Plotting for run ', run, ' complete.')
    
    #try epilog
    script <- NULL
    epimatch <- try(match.fun(epilog),silent=TRUE)
    if(is.function(epimatch))epilog <- epimatch
    else if (class(epilog)=='character'){
	    script <- epilog
	    epilog <- episcript
    }
    if (!is.null(epilog))if(is.function(epilog))try(
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
	    rundir=rundir,
	    ...,
	    script=script
	)
    )	    
}

#filters elipses for functions that do not accept them
safe.call <- function(what,...){
	extras <- list(...)
	legal <- names(formals(what))
	extras <- extras[names(extras) %in% legal]
	do.call(what=what,args=extras)
}

#creates a filepath from dir,run, and extension
filename <- function(dir,run=NULL,ext=NULL)file.path(dir,glue(run,ext))

#calculates a vector of cwres
getCwres <- function(directory){
    	cwrtab1 <- filename(directory, NULL, 'cwtab1.deriv')
        if (!file.exists(cwrtab1)) stop(cwrtab1,' does not exist.',call.=FALSE)
        tab.prefix <- filename(directory,NULL, '/cwtab')
	cwres.all<-compute.cwres(
		run.number=1,
		tab.prefix=tab.prefix,
		printToOutfile=TRUE
	)
	data.cwres <- read.table(
		file = filename(directory,NULL, '/cwtab1'), 
		skip = 1, 
		header = TRUE
	)
        data.cwres$CWRES
}

#loads non-null cwres onto tab file
setCwres <- function(cwres,file){
	cwres <- c('CWRES',cwres)
	tabdata<- readLines(file)[-1]
	if(length(tabdata)!=length(cwres))stop('cwres-tabfile length mismatch',call.=FALSE)
	msg <- paste('CWRES added', format(Sys.time(), '%a %b %d, %X, %Y'))
	tabdata <- paste(tabdata,cwres)
	tabdata <- c(msg,tabdata)
	writeLines(tabdata,con = file)
}


#finds the nonmem data set and coverts it to a covariate file
getCovs <- function(file,dir){
	    here <- getwd()
	    setwd(dir)
	    tryCatch( 
	    	suppressWarnings(
	    		covdata <- read.table(file = file, header = TRUE, as.is = TRUE, sep = ',')
	    	),
	    	error=function(e)stop(file,' not visible from ',dir,call.=FALSE),
	    	finally=setwd(here)
	    )	    
	    if('C' %in% names(covdata))covdata <- covdata[covdata$C != 'C',]
	    if(!'ID' %in% names(covdata))stop('ID not a column in ',file,call.=FALSE)
	    covdata <- covdata[!duplicated(covdata$ID),]
	    if(!nrow(covdata))stop(file,' has no rows',call.=FALSE)
	    return(covdata)
}

#returns the parameter file
getPars <- function(file){
	    if (!file.exists(file))stop(file,' does not exist.',call.=FALSE)
	    f <- read.table(file = file, header = TRUE, skip = 1)
	    if(!'ID' %in% names(f))stop('ID not a column in ',file,call.=FALSE)
            f <- f[!duplicated(f$ID),]
    	    if(!nrow(f))stop(file,' has no rows',call.=FALSE)
	    return(f)
}  

#scavenges the data set name/path from the control stream
getdname <- function(filename){
	    if(!file.exists(filename))stop(filename,' not found',call.=FALSE)
	    control <- read.nmctl(filename)
            if(!'data' %in% names(control))stop('data record not found in control stream')
            control$data <- sub('^ +','',control$data)#remove any leading spaces
            control$data <- control$data[!control$data=='']#remove any blank lines
            sub('^([^ ]+).*$','\\1',control$data[[1]])#take first line up to first space
}

#finds the tab file and reduces it to observation rows
getTabs <- function(file){
	    if (!file.exists(file))stop(file,' does not exist.',call.=FALSE)
	    tabdata <- read.table(file = file, header = TRUE, as.is = TRUE, skip = 1, comment.char = '')
	    if(!'EVID' %in% names(tabdata))stop('EVID not a column in ',file,call.=FALSE)
	    tabdata <- tabdata[tabdata$EVID == 0, ]
	    if(!nrow(tabdata))stop(file,' has no rows',call.=FALSE)
	    tabdata
}   

#melds the grpnames columns into one, renaming levels conditionally
groupnames <- function(data,grp,grpnames=NULL,run){
	    result <- factor(
	    	do.call(
  			paste,
				c(
					as.list(data[,grp,drop=FALSE]),
					sep=", "
				)
			)
		)
	   nlevs <- length(levels(result))
	   if(!is.null(grpnames))if(length(grpnames)==nlevs)levels(result) <- grpnames
	   if(!is.null(grpnames))if(length(grpnames)!=nlevs)warning(call.=FALSE,immediate.=TRUE,'Run ',run,' has ',nlevs,' grouping levels but ',length(grpnames),' grpnames (ignored).' )
	  result
}

#combines any number of tables using left join to load the columns specified in x
synthesis <- function(x,key=character(0),frames,...){
    if(any(sapply(frames,function(x)!inherits(x,'data.frame'))))stop()    
    x <- unique(x)
    y <- frames[[1]]
    frames[[1]] <- NULL
    x <- setdiff(x,names(y))
    while (length(x) & length(frames)){
	    z <- frames[[1]]
	    frames[[1]] <- NULL
	    z <- z[, union(key, intersect(x, names(z))),drop=FALSE]
        z <- z[!duplicated(z[, intersect(names(z), names(y))]),,drop=FALSE]
        if(length(setdiff(names(z),names(y)))) y <- stableMerge(y,z)
	    x <- setdiff(x,names(y))
    }
    y
}

#reduces a character vector to the subset found in options
strain <- function(x,options){
	    if(!is.null(x))x <- intersect(x,options)
            x
}

#back transforms cols in x
backtrans <- function(x,cols){
	    for(col in cols)x[[col]] <- exp(x[[col]])
	    x
}

#generates the plotting data set, given actual data frames, etc.
dataFormat <- function(
	tabdata,
	covdata,
	pardata,
	logtrans=FALSE,
	grp=NULL,
	grpnames=NULL,
	cont.cov=NULL,
	cat.cov=NULL,
	par.list=NULL,
	eta.list=NULL,
	missing=-99,
	run,
	...
){
    if (logtrans) tabdata <- backtrans(tabdata,intersect(names(tabdata),c('DV','PRED','IPRE','IPRED')))    
    available <- unique(c(names(tabdata),names(covdata),names(pardata)))
    grp <- strain(grp,available)
    cont.cov <- strain(cont.cov,available)
    cat.cov <- strain(cat.cov,available)
    par.list <- strain(par.list,available)
    eta.list <- strain(eta.list,available)
    findAnywhere <- c(grp,cont.cov,cat.cov)
    findInOutput <- c(par.list,eta.list)
    data <- synthesis(findAnywhere, key='ID',frames=list(tabdata,pardata,covdata),...)
    data <- synthesis(findInOutput, key='ID',frames=list(data,tabdata,pardata),...)
    missing <- as.numeric(as.character(missing))
    for(col in cont.cov) data[[col]] <- as.numeric(as.character(data[[col]]))
    for(col in cont.cov) data[[col]][!is.na(data[[col]]) & data[[col]]==missing] <- NA
    if(is.null(grp))data$grpnames <- 'all'
    if(is.null(grp))grp <- 'grpnames'
    data$grpnames <- groupnames(data,grp,grpnames,run)
    data
}

#generates the plotting data set, given project, run, etc.
dataSynthesis <- function(
	run, 
	project=getwd(), 
	logtrans = FALSE,
	grp = NULL, 
	grpnames = NULL,
	cont.cov = NULL,
	cat.cov = NULL,
	par.list = NULL,
	eta.list = NULL,
	missing = -99,
	rundir  = filename(project, run),
	ctlfile = filename(rundir,run,'.ctl'),
	outfile = filename(rundir,run,'.lst'),
	datfile = getdname(ctlfile),
	...
){
    #cleanup arguments
    force(datfile)
    if (!file.exists(outfile))stop(outfile,' does not exist.',call.=FALSE)
    if (!file.exists(ctlfile))stop(ctlfile,'does not exist.',.call.=FALSE)
    ctlfile <- read.nmctl(ctlfile)#switch from file name to file content
    outputrecords <- as.character(ctlfile[names(ctlfile)=='table'])
    tabfile <- tryCatch(tabfile(outputrecords,dir=rundir,...),error=function(e)stop('cannot locate *.tab in control stream for run ',run,call.=FALSE))
    parfile <- tryCatch(parfile(outputrecords,dir=rundir,...),error=function(e)stop('cannot locate *par.tab in control stream for run ',run,call.=FALSE))
    
    #acquire data
    tabdata <- getTabs(tabfile)  
    covdata <- getCovs(datfile,rundir)
    pardata <- getPars(parfile)
    
    #process data
    data <- dataFormat(
    	tabdata=tabdata,
    	covdata=covdata,
    	pardata=pardata,
    	logtrans=logtrans,
    	grp=grp,
    	grpnames=grpnames,
    	cont.cov=cont.cov,
    	cat.cov=cat.cov,
    	par.list=par.list,
    	eta.list=eta.list,
    	missing=missing,
    	run=run,
    	...
    )
    data
}

star <- function(x,y)gsub('*', y, x, fixed=TRUE)
plotfilename=function(
	run,
	dir=getwd(),
	grp=NULL,
	onefile=TRUE,
	stem='DiagnosticPlotReview',
	pext=if(onefile)'.pdf' else '_%03d.pdf',
	...
)filename(
	dir,
	glue(
		stem,
		paste(grp,collapse=''),
		'_',
		run
	),
	pext
)


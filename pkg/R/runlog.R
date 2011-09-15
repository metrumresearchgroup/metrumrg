as.pxml.ext <- function(file,lead=1,tag='param',...){
	if(!inherits(file,'connection'))if(!file.exists(file))stop(file, ' not found',call.=FALSE)
	x <- readLines(file)
	head <- seq(length.out=lead)
	note <- as.xml(x[head],tag='note')
	body <- tryCatch(
		as.data.frame.block(x[-head]),
		error=function(e)stop('cannot process ',file,call.=FALSE)
	)
	body <- as.xml(body[,-1],keyname=names(body)[[1]], key=body[[1]])
	nest(tag='param',x=c(note,body))
}
as.unilog.lst <- function(file,run,tool,...){
	if(!inherits(file,'connection'))if(!file.exists(file))stop(file,' not found',call. = FALSE)
	lines <- readLines(file)
	prob <- grep(pattern='^\\$PROB',lines,value=TRUE)[[1]]
	prob <- sub('\\$(PROB|PROBLEM) *','',prob,ignore.case=TRUE)
	prob <- gsub(',',';',prob)
	min <- 0**length(grep('MINIMIZATION SUCCESSFUL',lines))
	datafile <- getdname(file)
	res <- data.frame(
		stringsAsFactors=FALSE,
		tool=rep(tool,3),
		run=rep(run,3),
		parameter=c('prob','min','data'),
		moment=c('text','status','filename'),
		value=c(prob,min,datafile)
	)
	if(tool=='nm6') res <- res[res$parameter!='min',]
	res
}
as.unilog.pxml <- function(x,run,tool='nm7',...){
	if(is.null(x))return(unilog())
	est <- paste(x,collapse='\n')
	est <- gsub('\\(|\\)','',est)
	est <- gsub(',','.',est)
	tree <- xmlParse(est,asText=TRUE)
	xmlValue.NULL <- function(x,...)NA
	p <- do.call(
		rbind,
		xpathApply(
			tree,
			"/param/*[ITERATION]",
			fun=function(x)data.frame(
				stringsAsFactors=FALSE,
				parameter=xmlName(x),
				estimate=xmlValue(getNodeSet(x,"ITERATION[@key='-1000000000']/text()")[[1]]),
				se=xmlValue(getNodeSet(x,"ITERATION[@key='-1000000001']/text()")[[1]])
			)
		)
	)
	cov <- as.integer(all(is.na(p$se)))#1 if no covariance
	p$prse <- with(p, signif(digits=3, 100 * as.numeric(se)/abs(as.numeric(estimate))))
	#p$se <- NULL
	free(tree)
	p$prse <- as.character(p$prse)
	uni <- melt(p,id.var='parameter',variable_name='moment')
	uni[] <- lapply(uni,as.character)
	uni <- rbind(uni,data.frame(stringsAsFactors=FALSE,parameter='cov',moment='status',value=as.character(cov)))
	uni$tool <- tool
	uni$run <- run
	uni <- uni[,c('tool','run','parameter','moment','value')]
	uni$word <- sub('\\d.*','',uni$parameter)
	uni$number <- suppressWarnings(text2decimal(uni$parameter))
	known <- c('OBJ','THETA','OMEGA','SIGMA')
	uni$word[!uni$word %in% known] <- 'OTHER'
	uni$word <- factor(uni$word,levels=c(known,'OTHER'))
	uni <- sort(as.keyed(uni, c('tool','run','word','number','moment')))
	uni$word <- NULL
	uni$number <- NULL
	key(uni) <- c('tool','run','parameter','moment')
	uni <- uni[!(uni$parameter == 'OBJ' & uni$moment %in% c('prse','se')),]
	uni$parameter[uni$parameter=='OBJ'] <- 'ofv'
	uni$moment[uni$parameter=='ofv'] <- 'minimum'
	row.names(uni) <- NULL
	uni
}
as.unilog.run <- function(
	run,
	logfile='NonmemRunLog.csv',
	outfile=paste(run,'lst',sep='.'),
	extfile=file.path(
		dirname(outfile),
		paste(run,'ext',sep='.')
	),
	tool='nm6',
	...
){
	pars <- if(tool=='nm6')as.unilog.runlog(
		as.runlog.file(logfile)
	)else if(tool=='nm7')as.unilog.pxml(
		x=as.pxml.ext(extfile),
		run=run,
		tool=tool,
		...
	)
	requested <- 'cov' %in% names(read.nmctl(outfile))
	if(tool=='nm7')if(!requested) pars$value[pars$parameter=='cov' & pars$moment=='status'] <- '0'		
	other <- as.unilog.lst(file=outfile,run=run,tool=tool,...)
	rbind(pars,other)
}
#runlog has implicit columns:
#run, problem, rseflag, min, cov, mvof, p1...pn, run, (percent)
as.runlog.unilog <- function(x,...){
	if(!nrow(x))return(runlog())
	x$tool <- NULL
	x$precedent <- with(x,reapply(value,INDEX=list(run,parameter,moment),FUN=function(x)1:length(x)))
	regular <- c('prob','min','cov','ofv','data')
	scalar <- x[x$parameter %in% regular,]
	scalar$moment <- NULL
	if(any(duplicated(scalar[,c('run','precedent','parameter')])))stop('prob, min, cov, ofv should be unique within run')
	if(!all(scalar$moment[scalar$parameter=='ofv'] == 'minimum'))stop('ofv moment should be minimum')
	scalar <- data.frame(cast(scalar,run + precedent ~ parameter))
	names(scalar)[names(scalar)=='ofv'] <- 'mvof'
	for(col in regular)if(!col %in% names(scalar))scalar[[col]] <- NA
	scalar <- scalar[,c('run','precedent','prob','min','cov','mvof','data')]
	poly <- x[!x$parameter %in% regular,]
	pars <- unique(poly$parameter)
	#if(!all(poly$moment %in% c('estimate','prse')))stop('parameter moments should be estimate or prse')
	poly <- poly[poly$moment %in% c('estimate','prse'),]
	poly <- data.frame(cast(poly, run + precedent + moment ~ parameter))
	integral <- stableMerge(poly,scalar)
	integral <- integral[,c('prob','moment','min','cov','mvof',pars,'run','data')]
	integral$moment <- factor(integral$moment,levels=c('estimate','prse'),labels=c('','RSE'))
	integral	
}
#runlog is orthogonal and has header prob,moment,min,cov,mvof,P1 ... Pn, [run]
as.unilog.runlog <- function(x,tool='nm6',...){
	if(!nrow(x))return(unilog())
	regular <- c('prob','min','cov','mvof')
	if(!all(regular %in% names(x)))stop('runlog must have prob, min, cov, mvof')
	if(!'run' %in% names(x))x$run <- sub('^(RUN#? *)?([^ ]+)(.*$)','\\2',x$prob,ignore.case=TRUE)
	others <- setdiff(names(x),c(regular,'run'))
	spec <- c(regular,others,'run')
	x <- x[,spec]
	x$tool <- tool
	#x$moment <- as.character(factor(x$moment,levels=c('','RSE'),labels=c('estimate','prse')))
	x$moment[is.na(x$moment)] <- 'estimate'
	x$moment[x$moment=='RSE'] <- 'prse'
	x[] <- sapply(x,as.character)
	rmelt <- melt(x,id.var=c('tool','run','moment'),variable_name='parameter',na.rm=TRUE)
	rmelt$parameter <- as.character(rmelt$parameter)
	rmelt$value <- sub('^ *','',rmelt$value)
	rmelt$value <- sub(' *$','',rmelt$value)
	rmelt <- rmelt[with(rmelt,!(moment=='prse' & parameter %in% c('prob','min','cov','mvof'))),]
	rmelt$moment[rmelt$parameter %in% c('min','cov')] <- 'status'
	rmelt$moment[rmelt$parameter == 'prob'] <- 'text'
	rmelt$moment[rmelt$parameter == 'mvof'] <- 'minimum'
	rmelt$parameter[rmelt$parameter == 'mvof'] <- 'ofv'
	rmelt <- rmelt[,c('tool','run','parameter','moment','value')]
	rmelt
}
as.runlog.file <- function(file,...){
	if(!inherits(file,'connection'))if(!file.exists(file))stop(file, ' not found',call.=FALSE)
	r <- read.csv(file,header=FALSE,na.strings=c('.','NA',''),as.is=TRUE,...)
	names(r)[1:5] <- c('prob','moment','min','cov','mvof')
	last <- ncol(r)
	if(!any(is.na(r[[last]])))if(
		all(
			r[[last]]
				==
			sapply(
				strsplit(
					r$prob,
					' '
				),
				`[[`,
				1
			)
		)
	)names(r)[[last]] <- 'run'
	r
}
as.file.runlog <- function(x,file='NonmemRunLog.csv',header=FALSE,quote=FALSE,na='.',...)write.table(x,file=file,row.names=FALSE,col.names=header,sep=',',na='.',quote=FALSE,...)
unilog <- function()data.frame(
  	tool=character(0),
  	run=character(0),
  	parameter=character(0),
  	moment=character(0),
  	value=character(0)
)
runlog <- function()data.frame(
  	prob=character(0),
  	moment=character(0),
  	min=character(0),
  	cov=character(0),
  	mvof=character(0),
	run=character(0)
)


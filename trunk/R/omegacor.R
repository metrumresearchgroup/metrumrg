unilogcor <- function(
	pattern,
	run=0,
	project=getwd(),
	tool='nm7',
	extfile=file.path(project,run,paste(run,'ext',sep='.')),
	pxml=as.pxml.ext(extfile),
	unilog=as.unilog.pxml(x=pxml,run=run,tool=tool,...),
	...
){
	#ord.ceiling <- function(n)ceiling(sqrt(0.25+2*n)-0.5)
	startpattern <- glue('^',pattern)
	unilog <- unilog[with(unilog,contains(startpattern,parameter) & moment=='estimate'),]
	if(nrow(unilog)==0)stop('no estimates found for unilog parameters with pattern',startpattern)
	if(length(unique(unilog$run))!=1)stop('need exactly one unique value of run')
	unilog$value <- suppressWarnings(as.numeric(as.character(unilog$value)))
	unilog <- data.frame(cast(unilog,parameter~moment))
	indices <- as.character(text2decimal(as.character(unilog$parameter)))
	splits <- strsplit(indices,'.',fixed=TRUE)
	all <- unlist(splits)
	ord <- max(as.numeric(all))
	nms <- names(half(diag(ord)))
	nms <- glue(pattern,nms)
	val <- with(unilog, estimate[match(nms,parameter)])
	cov <- as.matrix(as.halfmatrix(val))
	cor <- cov2cor(cov)
	cor
}
	
omegacor <- function(run,project=getwd(),...)unilogcor(pattern='OMEGA',run=run,project=project,...)
sigmacor <- function(run,project=getwd(),...)unilogcor(pattern='SIGMA',run=run,project=project,...)


qstat <- function(u=Sys.info()[["login"]],f='',...){
	range <- c(
		'ext','cb','f','F','g','help','j','l','ne','pe',
		'pri','q','qs','r','s','t','U','u','urg','xml'
	)
	args <- c(list(u=u,f=f),list(...))
	args <- args[names(args) %in% range]
	if(length(args))names(args) <- glue('-',names(args))	
	vectors <- c(as.list(names(args)),args)
	vectors <- vectors[t(matrix(seq(length.out=length(vectors)),ncol=2))]
	string <- do.call(paste,vectors)
	command <- paste('qstat',string,command)
	command <- gsub('-[^ ]* NA','',command)
	safe.call(system,command=command,...)
}


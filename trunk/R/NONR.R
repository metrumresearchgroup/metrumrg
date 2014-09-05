`NONR` <-  
function (
	run, 
	...,
	command, 
	project = getwd(), 
	wait = TRUE,
	grid = FALSE, 
	concurrent = grid & wait,
	udef= FALSE, 
	invisible=udef,
	compile = TRUE,
	execute = TRUE,
	split = FALSE,
	checkrunno = TRUE, 
	checksum = TRUE, 
	diag = wait, 
	fdata = TRUE, 
	logtrans = FALSE,
	nice= TRUE, 
	epilog = NULL, 
	dvname = NULL, 
	grp = NULL, 
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99, 
	interface='autolog.pl',
	q='all.q',
	pe=NA
){
    if (win())  grid <- FALSE
    if (win())  concurrent <- FALSE
    if (length(run) > 99 & wait) warning('default installation will not wait for more than 99 runs')
    run <- unique(run)
    if (missing(command)){
    	message('argument "command" was not supplied')
    	message('searching for NONMEM ...')
    	candidate <- safe.call(findNonmemCommand,...)
    	if(!length(candidate))stop('NONMEM not detected. Specify "command" directly or see help for findNonmemCommand.')
    	if(length(candidate) > 1 ){
    		message('found: ',paste(collapse=', ',candidate))
    		candidate <- candidate[[1]]
    	}
    	#now candidate is length 1
    	message('using command: ',candidate)
    	command <- candidate
    }    
    args <- list(
	run = run,
	...,
	command = command, 
	project = project, 
	wait = wait,
	grid = grid, 
	# concurrent used only in this scope
	udef = udef, 
	invisible = invisible, 
	compile = compile,
	execute = execute,
	split = split,
	checkrunno = checkrunno, 
	checksum = checksum, 
	diag = diag, 
	fdata = fdata, 
	logtrans = logtrans, 
	nice = nice,
	epilog = epilog, 
	dvname = dvname, 
	grp = grp, 
	grpnames = grpnames, 
	cont.cov = cont.cov, 
	cat.cov = cat.cov, 
	par.list = par.list, 
	eta.list = eta.list, 
	missing = missing,
	interface = interface,
	q = q,
	pe = pe
    )
    res <- lapply(
    	run,
    	function(this,args,concurrent){
    		args$run <- this
    	if (concurrent & requireNamespace("fork", quietly = TRUE)){
            #library(fork)
            suppressWarnings(fork::handleSIGCLD())
            pid <- fork::fork(NULL)
            if (pid == 0) {
                tryCatch(
                	do.call("runNonmem", args),
                	error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
                )
                fork::exit()
            } else {
              pid
            }
        } else tryCatch(
        		do.call('runNonmem', args),
        		error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
        	)
    	},
    	args=args,
    	concurrent=concurrent
    )
    message("NONR complete.")
    invisible(res)
}
nix <- function().Platform$OS.type == 'unix'
win <- function().Platform$OS.type == 'windows'


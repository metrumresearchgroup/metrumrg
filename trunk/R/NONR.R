`NONR` <-  
function (
	run, 
	command, 
	project = getwd(), 
	boot = FALSE,
	grid = boot, 
	concurrent = grid,
	urgent = !boot,
	udef= FALSE, 
	invisible=udef,
	compile = TRUE,
	execute = TRUE,
	split = grid & compile & execute,
	checkrunno = TRUE, 
	checksum = TRUE, 
	diag = !boot, 
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
	delay = 0,
	verbose = TRUE,
	...
){
    if (win())  grid <- FALSE
    if (win())  concurrent <- FALSE
    run <- unique(run)
    if (missing(command)){
    	if(verbose)message('argument "command" was not supplied')
    	if(verbose)message('searching for NONMEM ...')
    	candidate <- safe.call(findNonmemCommand,...)
    	if(!length(candidate))stop('NONMEM not detected. Specify "command" directly or see help for findNonmemCommand.')
    	if(length(candidate) > 1 ){
    		if(verbose)message('found: ',paste(collapse=', ',candidate))
    		candidate <- candidate[[1]]
    	}
    	#now candidate is length 1
    	if(verbose)message('using command: ',candidate)
    	command <- candidate
    }    
    for (this in run) {
    	Sys.sleep(delay/1000)
        args <- list(
		run = this, 
		command = command, 
		project = project, 
		boot = boot,
		urgent = urgent,
		checkrunno = checkrunno, 
		diag = diag, 
		fdata = fdata, 
		epilog = epilog, 
		dvname = dvname, 
		logtrans = logtrans, 
		grp = grp, 
		grpnames = grpnames, 
		cont.cov = cont.cov, 
		cat.cov = cat.cov, 
		par.list = par.list, 
		eta.list = eta.list, 
		missing = missing,
		invisible = invisible, 
		checksum = checksum, 
		grid = grid, 
		nice = nice,
		udef = udef, 
		split = split,
		compile = compile,
		execute = execute,
		verbose = verbose,
		...
	)
        if (concurrent){
            library(fork)
            suppressWarnings(handleSIGCLD())
            pid <- fork::fork(NULL)
            if (pid == 0) {
                tryCatch(
                	if(verbose)do.call("runNonmem", args) 
                	else suppressMessages(suppressWarnings(do.call("runNonmem",args))),
                	#error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
                	error=function(e)if(verbose)writeLines(e$message,glue(this,'.nonr'))
                )
                exit()
            } else {
              if(verbose)message('launching run ',this,' on pid ',pid)
            }
        } else tryCatch(
        		if(verbose)do.call('runNonmem', args)
        		else suppressMessages(suppressWarnings(do.call("runNonmem",args))),
        		error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
        	)
    }
    if(verbose)message("NONR complete.")
}
nix <- function().Platform$OS.type == 'unix'
win <- function().Platform$OS.type == 'windows'
NONR72 <- function(
	run, 
	command, 
	project = getwd(), 
	boot = FALSE,
	grid = boot, 
	concurrent = grid,
	urgent = !boot,
	udef= FALSE, 
	invisible=udef,
	compile = TRUE,
	execute = TRUE,
	split = FALSE,
	checkrunno = TRUE, 
	checksum = TRUE, 
	diag = !boot, 
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
	delay = 0,
	verbose = TRUE, 
	...,
	interface='autolog.pl',
	q='all.q'
)NONR(
	run=run,
	command=command,
	project=project,
	boot=boot,
	grid=grid,
	concurrent=concurrent,
	urgent=urgent,
	udef=udef,
	invisible=invisible,
	compile=compile,
	execute=execute,
	split=split,
	checkrunno=checkrunno,
	checksum=checksum,
	diag=diag,
	fdata=fdata,
	logtrans=logtrans,
	nice=nice,
	epilog=epilog,
	dvname=dvname,
	grp=grp,
	grpnames=grpnames,
	cont.cov=cont.cov,
	cat.cov=cat.cov,
	par.list=par.list,
	eta.list=eta.list,
	missing= missing,
	delay = delay,
	verbose=verbose,
	...,
	interface=interface,
	q=q
)


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
	...
){
    if (win())  grid <- FALSE
    if (win())  concurrent <- FALSE
    run <- unique(run)
	    
    for (each in run) {
        args <- list(
		run = each, 
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
		...
	)
        if (concurrent){
            library(fork)
            suppressWarnings(handleSIGCLD())
            pid <- fork(NULL)
            if (pid == 0) {
                tryCatch(
                	do.call("runNonmem", args),
                	error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
                )
                exit()
            }
        } else tryCatch(
        		do.call('runNonmem', args),
        		error=function(e)warning(e$message,call.=FALSE,immediate.=TRUE)
        	)
    }
    message("NONR complete.")
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
	missing=-99,
	...,
	interface=interface,
	q=q
)


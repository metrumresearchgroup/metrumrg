addl <- function(x,...)UseMethod('addl')
addl.default <- function(x,interval=1,...){
	sentinel <-function(x,interval)!redundant(x,interval)
	region <-function(x,interval)cumsum(sentinel(x,interval))
	redundant <-function(x,interval){
  		res <- x-interval==prev(x)
  		res[is.na(res)] <- FALSE
  		res
	}
	following <-function(x,interval){
  		arg <- region(x,interval)
  		reapply(arg,arg,function(x)rev(seq_along(x)))-1
	}
	keep <- sentinel(x,interval)
  	addl <- following(x,interval)
  	ifelse(keep,addl,NA)
}
addl.data.frame <- function(
	x,
	interval=24,
	collapse=TRUE,
	cols=c('SUBJ','TIME','AMT','ADDL','II'),
	...
){
	stopifnot(length(cols)==5,length(interval)==1)
	subj <- cols[[1]]
	time <- cols[[2]]
	amt <- cols[[3]]
	addl <- cols[[4]]
	ii <- cols[[5]]
	calc <- reapply(
		x[[time]],
		cumsum(runhead(paste(x[[subj]],x[[amt]]))),
		addl.default,
		interval=interval
	)
	if(!addl %in% names(x))x[[addl]] <- 0
	if(!ii   %in% names(x))x[[ii]] <- 0
	stopifnot(length(calc)==nrow(x))
	if(any(!is.na(x[[addl]]) & x[[addl]] != 0 & !is.na(calc)))stop('attempt to recalculate ADDL')
	x[[addl]][!is.na(calc)] <- calc[!is.na(calc)]
	x[[ii]][!is.na(calc)] <- interval
	if(collapse)x <- x[!is.na(calc),]
	else rownames(x)[is.na(calc)] <- glue('-',rownames(x)[is.na(calc)])
	x
}
	

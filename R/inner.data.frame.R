inner <-
function(x,...)UseMethod('inner')

inner.data.frame <-
function(
	x,
	prob=0.95,
	tail=0.5*(1-prob),
	lo=tail,
	hi=prob+tail,
	include.lowest=TRUE,
	include.highest=TRUE,
	preserve=character(0),
	id.var=character(0),
	measure.var=setdiff(names(x),c(preserve,id.var)),
	na.rm=FALSE,
	...
){
	stopifnot(
		is.numeric(prob), prob <=1, prob >=0, 
		is.numeric(tail), tail <=1, tail >=0,
		is.numeric(lo),   lo   <=1, lo   >=0,
		is.numeric(hi),   hi   <=1, hi   >=0,
		is.logical(include.lowest), is.logical(include.highest),
		length(prob)==1, length(tail)==1, length(lo)==1, length(hi)==1, 
		length(include.lowest)==1, length(include.highest)==1,
		is.character(preserve),
		all(preserve %in% names(x)),
		is.character(measure.var),
		all(id.var %in% names(x)),
		is.character(measure.var),
		all(measure.var %in% names(x)),
		intersect(measure.var,id.var)==character(0),
		intersect(preserve,id.var)==character(0),
		intersect(preserve,measure.var)==character(0),
		!'inner.key' %in% names(x),
		!'inner.rownames' %in% names(x)
	)
	x$inner.rownames <- seq(length.out=nrow(x))
	ignore <- union(id.var,preserve)
	molten <- melt(x,id.var=c('inner.rownames',ignore),measure.var=measure.var,variable_name='inner.key')
	x$inner.rownames <- NULL
	molten$lo <- reapply(molten$value,INDEX=molten[,c('inner.key',id.var)],FUN=quantile,probs=lo,na.rm=na.rm)
	molten$hi <- reapply(molten$value,INDEX=molten[,c('inner.key',id.var)],FUN=quantile,probs=hi,na.rm=na.rm)
	molten$value[molten$value < molten$lo] <- NA
	molten$value[molten$value > molten$hi] <- NA
	if(!include.lowest) molten$value[molten$value==molten$lo] <- NA
	if(!include.highest) molten$value[molten$value==molten$hi] <- NA
	molten$lo <- NULL
	molten$hi <- NULL
	frozen <- data.frame(check.names=FALSE,cast(molten,inner.rownames + ... ~ inner.key))
	frozen <- frozen[,intersect(names(x),names(frozen))]
	frozen
}


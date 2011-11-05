lower <-
function(x,sd,interval=0.95, prob=0.5*(1-interval),...)sapply(
	seq_along(x),
	function(index)qnorm(
		p=prob,
		mean=x[index],
		sd=sd[index],
		...
	)
)
upper <-
function(x,sd,interval=0.95, prob=0.5*(1+interval),...)sapply(
	seq_along(x),
	function(index)qnorm(
		p=prob,
		mean=x[index],
		sd=sd[index],
		...
	)
)
bounds <-
function(x,sd,interval=0.95, lower=0.5*(1-interval), upper=0.5*(1+interval),left='(',right=')',sep=',',digits=3,...){
  stopifnot(interval<=1,interval>=0,lower<=1,lower>=0,upper<=1,upper>=0)
  lower <- signif(lower(x=x,sd=sd,interval=interval, prob=lower,...),digits=digits)
  upper <- signif(upper(x=x,sd=sd,interval=interval, prob=upper,...),digits=digits)
  glue(left,lower,sep,upper,right)
}


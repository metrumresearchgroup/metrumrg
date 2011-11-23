addl <-
function(x,interval=1){
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

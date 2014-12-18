`as.data.frame.block` <-
function(x,...){
	con <- textConnection(x)
	dat <- tryCatch(
		read.table(con,header=TRUE,as.is=TRUE,check.names=FALSE,...),
		error=function(e)stop('cannot process block',call.=FALSE),
		finally=close(con)
	)
	dat
}


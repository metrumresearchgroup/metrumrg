`as.xml` <-
function(x,...)UseMethod('as.xml')
`as.xml.character` <-
function(x,tag,...)glue(
	bracket(tag,...),
	x,
	bracket(tag,close=TRUE)
)
`as.xml.data.frame` <-
function(x,keyname='row',key=rownames(x),...){
	itemize <- function(col,frame,tag,key){
		values <- frame[,col]
		nest(as.xml(values,tag=tag,key=key),tag=col)
	}
	unlist(lapply(names(x),itemize,frame=x,tag=keyname,key=key))
}
`as.xml.default` <-
function(x,tag,...)as.xml(as.character(x),tag,...)


unitDensity <- function(x,...){
	res <- safe.call(density.default,x=x,...)
	res$y <- with(res, y/max(y,na.rm=TRUE))
	res
}
panel.densitystrip <- function(x,y,horizontal,col.line,fill,factor,border=col.line,col=fill,...){
	ordinal <- if(horizontal) x else y
	level <- if(horizontal) unique(y)[[1]] else unique(x)[[1]]
	data <- unitDensity(ordinal,...)
	data$y <- data$y * factor + level
	if(missing(col))col <- fill
	if(is.na(col))col <- fill
	if(horizontal)panel.polygon(x=data$x,y=data$y,border=border,col=col,...)
	else          panel.polygon(x=data$y,y=data$x,border=border,col=col,...)
}
panel.ref <- function(x,y,col='grey90',horizontal,rlim,...){
        x <- as.numeric(x)
        y <- as.numeric(y)
	if(horizontal)panel.rect(xleft=rlim[1],ybottom=0,xright=rlim[2],ytop=max(y) + 1,border='transparent',col=col)
	else panel.rect(xleft=0,ybottom=rlim[1],xright=max(x) + 1, ytop=rlim[2],border='transparent',
	col=col)
}
panel.cuts <- function(
	x,
	y,
	cuts,
	col,
	col.line,
	text=col.line,
	horizontal=TRUE,
	offset=-0.2,
	increment=0,
	format=function(x,...)as.numeric(round(x/sum(x)*100)),
	include.range=TRUE,
	zero.rm=TRUE,
	cex=0.7,
	...
){
	ordinal <- if(horizontal) x else y
	level <- if(horizontal) unique(y)[[1]] else unique(x)[[1]]
	cuts <- cuts[cuts >= min(ordinal,na.rm=TRUE) & cuts <= max(ordinal,na.rm=TRUE)]
	if(include.range) cuts <- c(range(ordinal),cuts)
	cuts <- sort(unique(cuts))
	midpoints <- (cuts[1:length(cuts)-1] + cuts[-1])/2
	count <- bin(ordinal,breaks=cuts,...)
	value <- format(count,...)
	if(zero.rm)value[value==0] <- NA
	value <- as.character(value)
	value[is.na(value)] <- ''
	level <- level + offset
	midpoints <- midpoints + increment
	if(horizontal)ltext(
		x=midpoints,
		y=rep(level,length(midpoints)),
		labels=value,
		cex=cex,
		col=text,
		...
	)
	else ltext(
		y=midpoints,
		x=rep(level,length(midpoints)),
		labels=value,
		cex=cex,
		col=text,
		...
	)	
}
panel.covplot <- function(
	x,
	y,
	ref=1,
	rlim=ref * c(0.75,1.25),
	cuts=ref * c(0.75,1,1.25),
	horizontal=TRUE,
	border='black',
	fill='grey',
	text='black',
	shade='grey90',
	col='white',
	...
){
	x <- as.numeric(x)
	y <- as.numeric(y)
	panel.ref(x,y,rlim=rlim,horizontal=horizontal,col=shade,...)
	panel.stratify(panel.levels=panel.densitystrip,  x=x,y=y,horizontal=horizontal,border=border,col=fill,...)
	panel.stratify(panel.levels=panel.cuts,          x=x,y=y,horizontal=horizontal,cuts=cuts,    text=text,...) 
	if(horizontal)args <- list(v=cuts,col=col,...)else args<-list(h=cuts,col=col,...)
	do.call(panel.abline,args)
	if(horizontal)args <- list(v=ref,...)else args<-list(h=ref,...)
	do.call(panel.abline,args)
}



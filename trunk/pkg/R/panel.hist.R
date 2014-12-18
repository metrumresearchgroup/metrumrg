`unitHist` <-
function(x,plot=FALSE,...){
	h <- hist(x,plot=plot,...)
	h$heights <- h$density/max(h$density)
	h
}
`panel.hist` <-
function(x,y,level,horizontal,col.line,fill,factor,border=col.line,col=fill,offset=-0.5,font,fontface,...){
	ordinal <- if (horizontal) x else y
	h <- unitHist(ordinal)
	h$heights <- h$heights * factor
	if(missing(col))col <- fill
	if(is.na(col))col <- fill

	if (horizontal) panel.rect(
		x=h$mids,
		y=rep(level+offset,length(h$mids)),
		width=(h$breaks[-1] - h$breaks[-length(h$breaks)]),
		height=h$heights,
		just=c('center','bottom'),
		border=border,
		col=col,
		...
	)
	else panel.rect(
		y=h$mids,
		x=rep(level+offset,length(h$mids)),
		height=(h$breaks[-1] - h$breaks[-length(h$breaks)]),
		width=h$heights,
		just=c('left','center'),
		border=border,
		col=col,
		...
	)
}
`panel.bar` <-
function(x,y,level,horizontal,col,col.line,fill,factor,font,fontface,...){
	if (horizontal)panel.segments(
		x0=x,
		y0=rep(level-factor,length(x)),
		x1=x,
		y1=rep(level+factor,length(x)),
		col=col.line,
		...
	)
	else panel.segments(
		x0=rep(level-factor,length(y)),
		y0=y,
		x1=rep(level+factor,length(y)),
		y1=y,
		col=col.line,
		...
	)	
}

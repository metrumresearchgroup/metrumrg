clear <- function(x,drop=NULL,...){
	if(!is.character(x))stop('x must be character')
	for(d in drop) x <- sub(d,'',x)
	x
}
ctl2xml <- function(x,...){
	doc <- nest(clear(x,drop=c('^[^;]*','^;','^[^<]*')),tag='document')
	doc[doc!='']
}
lookup.one <- function(x,within,by='name',as=NULL,type='parameter',...){
	stopifnot(length(x)==1,length(by)==1,length(as)<=1)
	if(length(x) !=1)stop('x must have length one')
	if(is.na(x))return(NA)
	if(length(by)!=1)stop('by must have length one')
	if(is.na(by))return(NA)
	tree <- xmlParse(within,asText=TRUE)
	dpath <- glue('//',type,'[@',by,'="',x,'"]/text()')
	apath <- glue('//',type,'[@',by,'="',x,'"]/@',as)
	result <- if(is.null(as))xpathSApply(tree,dpath,fun=xmlValue) else xpathSApply(tree,apath)
	free(tree)
	if(!length(result))return(as.character(NA))
	else return(result)
}
lookup <- function(x,within,by='name',as=NULL,type='parameter',...)sapply(
	x,
	lookup.one,
	within=within,
	by=by,
	as=as,
	type=type,
	...
)

#stuff <- c(
#'<parameter           label="a" num="1">monkey</parameter>',
#'<parameter name="y"            num="2">wombat</parameter>',
#'<parameter name="z" label="c" num="3"/>'
#)
#doc <- ctl2xml(stuff)
#lookup.one('y',within=doc)
#lookup.one('a',within=doc)
#lookup.one('a',within=doc,by='label',as='name')
#lookup.one('b',within=doc,by='label',as='name')
#lookup.one('c',within=doc,by='label',as='name')
#lookup.one(1,within=doc,by='num')
#lookup.one(2,within=doc,by='num')
#lookup.one(3,within=doc,by='num')
#lookup('y',within=doc)
#lookup('a',within=doc)
#lookup('a',within=doc,by='label',as='name')
#lookup('b',within=doc,by='label',as='name')
#lookup('c',within=doc,by='label',as='name')
#lookup(c('a','b','c'),within=doc,by='label',as='name')
#lookup(1:3,within=doc,by='num')
#lookup(3:1,within=doc,by='num')

#stuff <- c(
#'; etc <parameter name="x" label="a" num="1">monkey</parameter>',
#'and some other stuff',
#';<parameter name="y" label="b" num="2">wombat</parameter>',
#'with maybe a < sign',
#';<parameter name="z" label="c" num="3">coocoo</parameter>'
#)
#ctl2xml(stuff)









params <-
function(within,by='name',type='parameter',...){
	if(length(by)!=1)stop('by must have length one')
	if(is.na(by))return(NA)
	tree <- xmlParse(within,asText=TRUE)
	apath <- glue('//',type,'/@',by)
	result <- xpathSApply(tree,apath)
	free(tree)
	if(!length(result))return(character(0))
	return(result)
}


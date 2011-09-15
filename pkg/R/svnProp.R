isSubversionedFile <- function(file){
	stopifnot(
		is.character(file),
		length(file)==1
	)
	info <- system(paste('svn info',safeQuote(file)),intern=TRUE,ignore.stderr=TRUE)
	as.logical(length(info))
}
isSubversioned <- function(x,...)sapply(x,isSubversionedFile)
svnPropGetFile <- function(file,prop){
	stopifnot(
		is.character(file),
		is.character(prop),
		length(file)==1,
		length(prop)==1
	)
	if(!isSubversionedFile(file))return(NA)
	res <- system(paste("svn propget",prop,safeQuote(file)),intern=TRUE)
	if(length(res))return(res)else(return(''))
}
svnPropGet <- function(x,prop,...)sapply(x,svnPropGetFile,prop=prop)
svnPropSetFile <- function(file,prop,value){
	stopifnot(
		is.character(file),
		is.character(prop),
		is.character(value),
		length(file)==1,
		length(prop)==1,
		length(value)==1
	)
	system(paste("svn propset",prop,value,safeQuote(file)),intern=TRUE)
}
svnPropSet <- function(x,prop,value,...)sapply(x,svnPropSetFile,prop=prop,value=value)
svnMimeType <- function(x,...)svnPropGet(x,prop='svn:mime-type')
svnSetMimeType <- function(x,type,...)svnPropSet(x,prop='svn:mime-type',value=type)
svnIsText <-function(x,...){#http://subversion.apache.org/faq.html#binary-files
	type <- svnMimeType(x,...)
	none <- type == ''
	text <- contains('^text/',type)
	xbit <- type == 'image/x-xbitmap'
	xpix <- type == 'image/x-xpixmap'
	none | text | xbit | xpix
}
svnMarkAsText <- function(x,...)svnPropSet(x,prop='svn:mime-type',value='text/')
svnMarkAsNonText <- function(x,...)svnPropSet(x,prop='svn:mime-type',value='application/octet-stream')

	

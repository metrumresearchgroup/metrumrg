metrumrgURL <-
function(url,lib.loc=NULL,browser=getOption("browser"),encodeIfNeeded=FALSE){
	stopifnot(length(url)==1)
	file <- system.file(url,package='metrumrg',lib.loc=lib.loc,mustWork=TRUE)
	path <- glue('file://',file)
	browseURL(path,browser=browser,encodeIfNeeded=encodeIfNeeded)
}


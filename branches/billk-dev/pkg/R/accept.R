`packageCheck` <-
function(x,lib.loc=NULL){
	if(!is.character(x))stop("x must be character")
	if(length(x)!=1)stop("x must be atomic")
	success <- library(x,character.only=TRUE,lib.loc=lib.loc,logical.return=TRUE)
	if(!success){
		return(0)
	}
	testResult <- try(suppressWarnings(example(x,local=TRUE)))
	if(inherits(testResult,"try-error")){
		return(0)
	}
	return(packageDescription(x,lib.loc=lib.loc,fields="Version"))
}
`accept` <-
function(
	contingencies=c(
		"akima",
		"boot",
		"coda",
		"chron",
		"foreign",
		"fork",
		"lattice",
		"locfit",
		"MASS",
		"nlme",
		"plyr",
		"R2WinBUGS",
		"reshape",
		"SASxport",
		"survival"
	),
	installMissing=TRUE,
	...,
	lib.loc=.libPaths(),
	lib=lib.loc[[1]],
	type='source'
){
	check <- NULL
	stopifnot(length(lib) == 1)
	if(!is.null(contingencies)){
		if(!inherits(contingencies,"character"))stop("contingencies must be a character vector of package names, or NULL")
		if(installMissing){
			installed <- installed.packages(noCache=TRUE,lib.loc=lib.loc)[,1]
			missing <- setdiff(contingencies,installed)
			if(length(missing))try(
				install.packages(missing,lib=lib,type=type)
			)
		}
		check <- sapply(contingencies,packageCheck,lib.loc=lib.loc)
		if(any(check==0))stop(paste("check failed for",paste(names(check)[check==0],collapse=", ")))
	}

	installed <- installed.packages(noCache=TRUE,lib.loc=lib.loc)
	acceptor <- Sys.info()["login"]
	time <- Sys.time()
	
	filepath <- file.path(.Library,"accept.xml")
	
	as.XMLNode <- function(x,...)UseMethod("as.XMLNode")
	as.XMLNode.XMLDocument <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	as.XMLNode.character <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	if(file.exists(filepath))doc <- as.XMLNode.XMLDocument(filepath,asText=FALSE)
	else doc <- as.XMLNode("<acceptance/>")
	accepted <- newXMLNode("accepted")
	accepted <- addChildren(accepted,newXMLNode("acceptor",acceptor))
	accepted <- addChildren(accepted,newXMLNode("time",as.character(time)))
	
	for (package in names(check)){
		test <- newXMLNode("test")
		test <- addChildren(test,newXMLNode("package",package))
		#test <- addChildren(test,newXMLNode("version",check[package]))
		test <- addChildren(test,newXMLNode("version",installed[package,'Version']))
		test <- addChildren(test,newXMLNode("path",installed[package,'LibPath']))
		accepted <- addChildren(accepted,test)
	}
	doc <- addChildren(doc,as.XMLNode(saveXML(accepted),asText=TRUE))
	saveXML(doc,filepath)
	invisible(check)
}
`acceptance` <-
function(){
	filepath <- file.path(.Library,"accept.xml")
	as.XMLNode <- function(x,...)UseMethod("as.XMLNode")
	as.XMLNode.XMLDocument <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	as.XMLNode.character <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	if(file.exists(filepath))return(as.XMLNode.XMLDocument(filepath,asText=FALSE))
	return(NULL)
}


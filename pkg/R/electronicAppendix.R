electronicAppendix <- function(
	x,
	as,
	pattern=NULL,
	recursive=TRUE,
	ignore.case=TRUE,
	zip=FALSE,
	...
){
	stopifnot(length(x)==1,length(as)==1)
	if(!file.exists(x))stop('cannot find ',x)
	if(contains('\\.zip$',as,ignore.case=TRUE))stop("'as' must be specified as a directory")
	zipname <- glue(as,'.zip')
	if(!zip & file.exists(as))stop(as,' already exists')#only tolerated if zip is true
	conflict <- file.exists(as)
	if(zip & file.exists(zipname))stop(zipname,' already exists')
	tmpdir <- glue(as,'_tmp')
	if(file.exists(tmpdir))unlink(tmpdir,recursive=TRUE)
	files <- dir(
		path=x,
		pattern=pattern,
		recursive=recursive,
		ignore.case=ignore.case
	)
	system(paste('svn export',safeQuote(x),tmpdir))
	local <- file.path(x,files)
	foreign <- file.path(tmpdir,files)
	txt <- svnIsText(local)
	current <- contains('\\.txt',local,ignore.case=TRUE)
	change <- foreign[ 
		file.exists(foreign) & 
		!is.na(txt) & #maybe redundant, as files not subversioned (will have na txt but) will not be exported.
		txt &
		!current
	]
	append.txt <- function(x)file.rename(x,glue(x,'.txt'))
	sapply(change,append.txt)
	setaside <- glue(as,'_EA_bak')
	if(conflict)file.rename(from=as,to=setaside)
	file.rename(from=tmpdir,to=as)
	tryCatch(
		if(zip){
			system(paste('zip -r',zipname,as))
			unlink(as,recursive=TRUE)
		},
		error=function(e)stop('cannot zip or unlink ',as,call.=FALSE),
		finally=if(conflict)file.rename(from=setaside,to=as)
	)
}
	
	
	
electronicAppendix <- function(
	x,
	as=glue(x,'_EA'),
	pattern=NULL,
	recursive=TRUE,
	ignore.case=TRUE,
	zip=FALSE,
	at=numeric(0),
	...
){
	stopifnot(length(x)==1,length(as)==1,length(at) <= 1)
	rev <- if (length(at)) glue('@',at) else ''
	if(!file.exists(x))stop('cannot find ',x)
	if(contains('\\.zip$',as,ignore.case=TRUE))stop("'as' must be specified as a directory")
	zipname <- glue(as,'.zip')
	if(!zip & file.exists(as))stop(as,' already exists')#only tolerated if zip is true
	conflict <- file.exists(as)
	if(zip & file.exists(zipname))stop(zipname,' already exists')
	tmpdir <- glue(as,'_tmp')
	if(file.exists(tmpdir))unlink(tmpdir,recursive=TRUE)
	system(paste('svn export',safeQuote(glue(x,rev)),tmpdir)) # safeQuote(x) if head rev.
	files <- dir(
		#path=x,
		path=tmpdir,
		pattern=pattern,
		recursive=recursive,
		ignore.case=ignore.case
	)
	#system(paste('svn export',safeQuote(x),tmpdir))
	local <- file.path(x,files) # files from tmpdir, as though they live in x
	local <- glue(local,rev) # if no 'at', these are indeed the export targets and do exist
	foreign <- file.path(tmpdir,files)
	txt <- svnIsText(local) # does a local lookup if no rev, else checks repository. 
	# In the first case, the file must be present locally since it was exported from local.
	# In the second case, file must be present in repo (where checked) since it was exported from repo.
	# So txt should never be NA
	if(any(is.na(txt))) warning('cannot determine text status for one or more files, e.g.', local[is.na(txt)][[1]])
	current <- contains('\\.txt',local,ignore.case=TRUE)
	change <- foreign[ 
		# file.exists(foreign) & # maybe now unnecessary, since foreign must exist, given "files" scavenged from "tmpdir"
		# !is.na(txt) & #maybe redundant, as files not subversioned (will have na txt but) will not be exported.
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
	
	
	
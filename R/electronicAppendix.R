electronicAppendix <- function(
	x,
	as=NULL,
	pattern=NULL,
	recursive=TRUE,
	ignore.case=TRUE,
	zip=FALSE,
	at=numeric(0),
	...
){
	checkout <- !!length(at)
	stopifnot(length(x)==1,length(as)<=1,length(at) <= 1)
	x <- file.path(dirname(x),basename(x))
	rev <- if (checkout) glue('@',at) else '' # filename variant
	rev2 <- if (checkout) glue('rev',at) else '' # export variant
	if(is.null(as)) as = glue(x,rev,'_EA')
	if(!file.exists(x))stop('cannot find ',x)
	if(contains('\\.zip$',as,ignore.case=TRUE))stop("'as' must be specified as a directory")
	zipname <- glue(as,'.zip')
	if(!zip & file.exists(as))stop(as,' already exists')#only tolerated if zip is true
	if(zip & file.exists(zipname))stop(zipname,' already exists')
	tmpdir <- glue(as,'_tmp')
	if(file.exists(tmpdir))unlink(tmpdir,recursive=TRUE)
	info <- suppressWarnings(system(paste('svn info --xml',safeQuote(x)),intern=TRUE,ignore.stderr=TRUE))
	info <- paste(info, collapse='')
	tree <- xmlParse(info)
	url <- xmlValue(getNodeSet(tree,"/info/entry/url/text()")[[1]])
	url <- glue(url,rev)
	method <- if(checkout) 'co' else 'export'
	source <- if(checkout) url else x
	source <- safeQuote(source)
	system(paste('svn',method,source,tmpdir)) # safeQuote(x) if head rev.
	files <- dir(
		#path=x,
		path=tmpdir,
		pattern=pattern,
		recursive=recursive,
		ignore.case=ignore.case
	)
	local <- file.path(x,files) # files from tmpdir, bearing context with svn info
	#local <- glue(local,rev) # if no 'at', these are indeed the export targets and do exist
	foreign <- file.path(tmpdir,files)
	txt <- svnIsText(if(checkout) foreign else local) # does a lookup wherever svn info is 
	# If head, the file must be present locally since it was exported from local.
	# If old rev, file must be present in foreign (where checked) since it was checked out from repo.
	# So txt should never be NA
	# Now that we have mimetype info, we reduce foreign to mere export by removing .svn
	dirs <- list.dirs(tmpdir)
	svn <- dirs[basename(dirs) == '.svn']
	unlink(svn,recursive=TRUE)
	if(any(is.na(txt))) warning('cannot determine text status for one or more files, e.g.', local[is.na(txt)][[1]])
	current <- contains('\\.txt$',foreign,ignore.case=TRUE)
	change <- foreign[ 
		# file.exists(foreign) & # maybe now unnecessary, since foreign must exist, given "files" scavenged from "tmpdir"
		# !is.na(txt) & #maybe redundant, as files not subversioned (will have na txt but) will not be exported.
		txt &
		!current
	]
	append.txt <- function(x)file.rename(x,glue(x,'.txt'))
	sapply(change,append.txt)
	setaside <- glue(as,'_bak')
	conflict <- file.exists(as)
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
	
	
	
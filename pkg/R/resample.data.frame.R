`resample` <-
function(x,...)UseMethod("resample")
`resample.data.frame` <-
function(
	x, 
	names, 
	key=NULL, 
	rekey=FALSE, 
	out=NULL, 
	stratify=NULL, 
	ext='.csv',
	row.names=FALSE,
	quote=FALSE,
	sep=',',
	replace=TRUE,
	...
){
dat <- x
if(!is.null(out))out <- sub("/$","",out)
if(is.null(key))dat$resample.rownames <- rownames(dat)
if(is.null(key))key <- "resample.rownames"
if(length(key)!=1)stop('length of key must be one')
if(!key %in% names(dat))stop('key must name a column in x')
if(is.null(stratify))stratify <- rep(0, nrow(dat))
if(is.character(stratify) && all(stratify %in% names(dat)))stratify <- dat[stratify]
if(!is.list(stratify)) stratify <- list(stratify)
stratify <- as.data.frame(stratify)
"%nests%" <- function(a,b)length(unique(interaction(interaction(a),interaction(b))))==length(unique(interaction(b)))
if(!stratify %nests% dat[[key]])stop("key not nested within stratification levels")
ind.key <- dat[[key]][!duplicated(dat[[key]])]
ind.strat <- stratify[!duplicated(dat[[key]]),]
bins <- split(ind.key,f=ind.strat,drop=TRUE)
rowsets <- split(rownames(dat),dat[[key]]) 
doBin <- function(bin,replace,...){
	if(length(bin)==1)return(bin)
	return(safe.call(sample,x=bin,replace=replace,...))
}
doName <- function(name,replace,...) {
        sample.id <- unlist(sapply(bins,doBin,replace=replace,...))
        sample.rownames <- rowsets[as.character(sample.id)]
        sample.dataset <- dat[(unlist(sample.rownames)), ]
        if (rekey) 
            sample.dataset[[key]] <- rep(1:length(sample.rownames), 
                times = sapply(sample.rownames, length))
        sample.dataset$resample.rownames <- NULL
        if (is.null(out)) 
            return(sample.dataset)
        write.table(sample.dataset, file = glue(out, "/", name, 
            ext), row.names = row.names, quote = quote, sep=sep,...)
        return(nrow(sample.dataset))
}
invisible(lapply(as.list(as.character(names)), doName, replace=replace,...))
}
`resample.csv.filename` <-
function(x,...){
extras <- list(...)
file <- list(file=x)
valid <- c(file,extras[names(extras) %in% names(formals(read.table))])
dat <- do.call("read.csv",args=valid)
resample(dat,...)
}

`resample.filename` <-
function(x,...){
extras <- attr(x,'extras')
file <- list(file=x)
valid <- c(file,extras[names(extras) %in% names(formals(read.table))])
dat <- do.call("read.table",args=valid)
resample(dat,...)
}


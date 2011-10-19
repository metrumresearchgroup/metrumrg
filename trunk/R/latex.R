#quoting from http://en.wikibooks.org/wiki/LaTeX/Absolute_Beginners ...
#"Anything in LaTeX can be expressed in terms of commands and environments."

#LaTeX Commands
#LaTeX commands are case sensitive, and take one of the following two formats:
#	▪	They start with a backslash \ and then have a name consisting of letters only. Command names are terminated by a space, a number or any #other "non-letter".
#	▪	They consist of a backslash \ and exactly one non-letter.
#Some commands need an argument, which has to be given between curly braces { } after the command name. Some commands support optional #parameters, which are added after the command name in square brackets [ ]. The general syntax is:
#\commandname[option1,option2,...]{argument1}{argument2}...

#LaTeX environments
#Environments in LaTeX have a role that is quite similar to commands, but they usually have effect on a wider part of the document. Their syntax #is:
#\begin{environmentname}
#text to be influenced
#\end{environmentname}
#Between the \begin and the \end you can put other commands and nested environments. In general, environments can accept arguments as well, but #this feature is not commonly used and so it will be discussed in more advanced parts of the document.
is.alpha <- function(x,...){
	stopifnot(is.character(x))
	font <- c(letters,LETTERS)
	x <- strsplit(x,'')
	sapply(x,function(vec)all(vec %in% font))
}
is.one.nonalpha <- function(x,...){
	stopifnot(is.character(x))
	font <- c(letters,LETTERS)
	(nchar(x)==1) & (!x %in% font)
}
is.latex.token <- function(x,...)is.alpha(x) | is.one.nonalpha(x)
spaces <- function(x)paste(rep(' ',x),collapse='')
tagvalue <- function(x,sep='=',collapse=',',...){
	stopifnot(is.list(x))
	y <- sapply(
		seq_along(x),
		function(num){
			tag <- names(x)[[num]]
			value <- x[[num]]
			if(!is.null(tag)) value <- paste(tag,value,sep=sep)
			value
		}
	)
	paste(y,collapse=collapse)
}
latex.options <- function(x,...){
	x <- as.list(x)
	x <- tagvalue(x)
	if(nchar(x)) x <- glue('[',x,']')
	x
}
latex.args <- function(x,...){
	x <- sapply(x,function(x)glue('{',x,'}'))
	x <- paste(x,collapse='')
	x
}	
command <- function(x, options=NULL, args=NULL,depth=0){
	stopifnot(length(x)==1, is.latex.token(x))
	options <- latex.options(options)
	args <- latex.args(args)
	res <- paste(c(spaces(depth),'\\',x,options,args),collapse='')
	res
}
wrap <- function(x,environment,options=NULL,args=NULL,depth=0){
	stopifnot(is.character(x),length(environment)==1,is.latex.token(environment))
	options <- latex.options(options)
	args <- latex.args(args)
	begin <- glue(spaces(depth),'\\begin{',environment,'}',options,args)
	end   <- glue(spaces(depth),'\\end{',environment,'}')
	x <- glue(spaces(depth+1),x)
	res <- c(begin,x,end)
	res
}
breaks <- function(x,...)as.integer(runhead(x))[-1]
tabularformat <- function(justify,breaks,walls){
	stopifnot(
		length(walls)==2,
		length(justify)==length(breaks)+1,
		is.numeric(breaks),
		is.numeric(walls),
		!any(is.na(breaks)),
		!any(is.na(walls))
	)
	format <- ''
	format <- append(format, rep('|',walls[[1]]))
	for(i in seq_along(breaks)){
		format <- append(format,justify[[i]])
		format <- append(format,rep('|',breaks[[i]]))
	}
	#since there is one more justify than breaks ...
	format <- append(format, rev(justify)[[1]])
	format <- append(format, rep('|',walls[[2]]))
	format <- paste(format,collapse='')	
	format
}
row2tabular <- function(x,...){
	x <- paste(x,collapse=' & ')
	x <- paste(x,'\\\\')
	x
}
align.decimal <- function(x,decimal.mark='.',...){
	x <- prettyNum(x)
  nodecimal <- !contains(decimal.mark,x,fixed=TRUE)
  x[nodecimal] <- glue(x[nodecimal],' ')
	splits <- strsplit(x,decimal.mark,fixed=TRUE)
	splits <- lapply(splits,function(x){if(length(x)==1)x[[2]]<-'';x})
	tails <- sapply(splits,function(x)nchar(x[[2]]))
	need <- max(tails) - tails
	while(any(need > 0)){
		x[need > 0] <- glue(x[need > 0],' ')
		need <- need - 1
	}
	x
}
ltable <- function(x,...)UseMethod('ltable')
ltable.data.frame <- function(
	x,
	caption=NULL,
	cap=caption,
	cap.top=TRUE,
	label=NULL,
	options='!htpb',
	environments='center',
  file=NULL,
	...
){
	x <- tabular(x,...)
	for (env in environments) x <- wrap(x,env)
  if (!is.null(label))label <- command('label',args=label)
	if(!is.null(caption)){
		caption <- command(
      'caption',
      options=cap,
      args=paste(caption,label)
		)
		x <- c(
			if(cap.top)caption else NULL,
			x,
			if(!cap.top)caption else NULL
		)
	}
	x <- wrap(
		x,
		'table',
		options=options
	)
	if(is.null(file))return(x)
	else {
		writeLines(x,file)
		invisible(x)
	}
}	
tabular <- function(x,...)UseMethod('tabular')
tabular.data.frame <- function(
	x,
	rules=c(2,1,1),
	walls=0,
	grid=FALSE,
	rowgroups=rownames(x),
	colgroups=names(x),
	rowbreaks=if(grid)breaks(rowgroups,...)else 0,
	colbreaks=if(grid)breaks(colgroups,...)else 0,
	charjust='left',
	numjust='right',
	justify=ifelse(sapply(x,is.numeric),numjust,charjust),
	colwidth=NA,
	paralign='top',
	na='',
	verbatim=ifelse(sapply(x,is.numeric),TRUE,FALSE),
	escape='#',
	trim=TRUE,
	...
){
	#groom arguments
	rules <- rep(rules, length.out=3)
	walls <- rep(walls, length.out=2)
	rowgroups <- rep(rowgroups, length.out=nrow(x))
	colgroups <- rep(colgroups, length.out=ncol(x))
	rowbreaks <- rep(rowbreaks, length.out=nrow(x)-1)
	colbreaks <- rep(colbreaks, length.out=ncol(x)-1)
	stopifnot(length(charjust)==1)
	stopifnot(length(numjust)==1)	
	stopifnot(length(escape)==1)	
	stopifnot(charjust %in% c('left','right','center'))
	stopifnot(numjust %in% c('left','right','center'))
	na <- rep(na, length.out=ncol(x))
	verbatim <- as.logical(rep(verbatim, length.out=ncol(x)))
	paralign <- map(paralign,from=c('top','middle','bottom'),to=c('p','m','b'))[[1]]
	colwidth <- rep(colwidth, length.out=ncol(x))
	colwidth <- sub('^',glue(paralign,'{'),colwidth)
	colwidth <- sub('$','}',colwidth)
	justify <- rep(justify, length.out=ncol(x))
	decimal <- justify=='decimal'
	justify <- map(justify, from=c('left','right','center','decimal'),to=c('l','r','c','r'))
	justify[!is.na(colwidth)] <- colwidth[!is.na(colwidth)]
	format <- tabularformat(justify=justify, breaks=colbreaks, walls=walls) #ready
	header <- row2tabular(names(x)) #ready
	sapply(names(x)[verbatim],function(nm)if(any(!is.na(x[[nm]]) & contains(escape,x[[nm]],fixed=TRUE)))warning(nm,'contains', escape))
	x[] <- lapply(seq_along(x),function(col)if(decimal[[col]])align.decimal(x[[col]],...)else format(x[[col]],trim=trim,...))
	x[] <- lapply(seq_along(x),function(col)sub('^ *NA *$',na[[col]],x[[col]]))
	x[] <- lapply(seq_along(x),function(col)if(verbatim[[col]])glue('\\verb',escape,x[[col]],escape)else x[[col]])
	x <- as.matrix(x)
	x <- apply(x,1,row2tabular) #ready
	# we now have a format string, a header, and all rows as character vector
	# splice in the horizontal rules
	# we treat header as a row, and treat rules[2:3] as pertaining to first and last row.
	# we create an empty row to represent pre-header
	x <- c('',header,x)
	rowbreaks <- c(rules[1:2],rowbreaks,rules[[3]])
	stopifnot(length(rowbreaks)==length(x))
	while(any(rowbreaks > 0)){
		x[rowbreaks > 0] <- paste(x[rowbreaks > 0],'\\hline')
		rowbreaks <- rowbreaks - 1
	}
	res <- wrap(x,'tabular',args=format)
	res
}
metaSub <- function(x,...)UseMethod("metaSub")

metaSub.filename <- 
function (x, names, pattern=NULL, replacement=NULL,...){
       if(!length(x) %in% c(1,length(names)))stop("x must be scalar, or as long as 'names'")
       if(!is.null(pattern))
	       if(length(pattern)>0)
		       for(i in 1:length(pattern))
			       if(!length(pattern[[i]]) %in% c(1,length(names)))
				       stop('pattern elements must be scalar or as long as "names"')
       if(!is.null(replacement))
	       if(length(replacement)>0)
		       for(i in 1:length(replacement))
			       if(!length(replacement[[i]]) %in% c(1,length(names)))
				       stop('replacement elements must be scalar or as long as "names"')
       choose <- function(set,i)if(length(set)==1)set else set[[i]]
       invisible(lapply(
       	   seq(length.out=length(names)),
	   function(i,x,names,pattern,replacement,...)metaSub(
	   	paste(
			readLines(
				choose(x,i)
			),
			collapse='\n'
		),
		names=choose(names,i),
		pattern=if(is.null(pattern))NULL else lapply(pattern,choose,i),
		replacement=if(is.null(replacement))NULL else lapply(replacement,choose,i),
		...
	  ),
	  x=x,
	  names=names,
	  pattern=pattern,
	  replacement=replacement,
	  ...
       ) )                                                                                                                                                 
 }
# metaSub(
# 	as.filename(c('1001.ctl','1002.ctl')),
#	names=c('a','b'),
#	out='.',
#	pattern=list(1001:1002),
#	replacement=list(c('a','b'))
#)
`metaSub.character` <-
function (x, names, pattern = NULL, replacement = NULL, out = NULL, 
    suffix = ".txt",fixed=TRUE,...) 
{
    names <- as.character(names)
    if (!is.null(out)) 
        out <- sub("/$", "", out)
    #input checking
    if (length(names)!=length(unique(names))) stop ("names must be unique")
    #Since expressions have list behaviors, they must be explicitly enclosed in lists to prevent unintended subsetting.
    if (is.expression(replacement)) replacement <- list(replacement)#trap bare expression at first level
    if (!is.null(replacement)) replacement[sapply(replacement,is.expression)] <- lapply(replacement[sapply(replacement,is.expression)],list)#trap bare expression at second level
    if (length(pattern) != length(replacement)) stop("lengths of pattern and replacement must match")
    if (!all(sapply(pattern, FUN = length) %in% c(1, length(names)))) stop("elements in pattern must be atomic, or of same length as names")
    if (!all(sapply(replacement, FUN = length) %in% c(1, length(names)))) stop("elements in replacement must be atomic, or of same length as names")
    if (!length(fixed) %in% c(1,length(pattern)))stop("fixed must be atomic, or of same length as pattern")
    #canonical forms
    if (!is.list(pattern)) pattern <- as.list(pattern)
    if (!is.list(replacement)) replacement <- as.list(replacement)
    if (length(fixed)==1)fixed <- rep(fixed,length(pattern))
    if (length(x)==0)stop("x has no length")
    if (length(x)>1) x <- paste(x,collapse="\n")
    
    #main function
    rewrite <- function(name,names,...) {
        result <- x
        limit <- function(vec, name,names) {
            if (length(vec) > 1) vec <- vec[names==name]
            vec[[1]]
        }
        pattern     <- lapply(pattern, limit, name, names)
        replacement <- lapply(replacement, limit, name, names)
        fixed       <- lapply(fixed, limit, name, names)
        if(length(replacement)) replacement[sapply(replacement,is.expression)] <- lapply(
        	replacement[sapply(replacement,is.expression)],
        	eval,
        	envir=c(
        		list(name=name),
        		list(...)
        	)
        )
        replacement <- sapply(replacement, as.character)
        replacement <- gsub(pattern = "\\\\\\*", replacement = '_star_', x = replacement)
        replacement <- gsub(pattern = "\\*", replacement = name, x = replacement)
        replacement <- gsub(pattern = "_star_", replacement = '*', x = replacement)
        for (i in seq(pattern)) result <- do.call(
        	gsub,
        	c(
        		list(
        			pattern=pattern[[i]], 
        			replacement=replacement[[i]], 
            		x=result,
            		fixed=fixed[[i]]
				),
				list(...)[names(list(...)) %in% names(formals(gsub))]
        	)
        )
        if (!is.null(out)) writeLines(
		result, 
		con = paste(
			out,
			"/",
			name,
			suffix,
			sep = ""
		)
	)
        return(result)
    }
    invisible(sapply(X=names, FUN=rewrite, names=names, ...))
}


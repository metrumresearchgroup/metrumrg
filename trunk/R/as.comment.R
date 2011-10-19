`as.comment` <- function(x,...)UseMethod("as.comment")

`as.comment.default` <- function(x,...){
	x <- as.logical(x)
	class(x) <- c("comment",class(x))
	x
}

`as.comment.comment` <- function(x,...)x

`as.character.comment` <- function(x,...)format(x)

`c.comment` <- function(...,recursive=FALSE)structure(c(unlist(lapply(list(...), unclass))), class="comment")

`as.data.frame.comment` <- function (x, row.names = NULL, optional = FALSE, ...) 
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    if (is.null(row.names)) {
        if (nrows == 0) 
            row.names <- character(0)
        else if (length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
        }
        else row.names <- .set_row_names(nrows)
    }
    names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

`[.comment` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

`[[.comment` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

`rep.comment` <- function(x,...){
	y <- NextMethod()
	class(y) <- class(x)
	y
}

`format.comment` <- function(x,...)unclass(ifelse(x,"C","."))

`print.comment` <- function(x,...){
	print(format(x),...,quote=FALSE)
	invisible(x)
}
xtfrm.comment <- function(x)as.numeric(x)

`hide` <- function(x,...)UseMethod("hide")
`hide.data.frame` <- function(x,where,why,...){
	if(!"C" %in% names(x))x$C <- as.comment(FALSE)
	where <- as.logical(where)
	x$C[where] <- TRUE
	if(!why %in% names(x)){
		x[[why]] <- as.flag(0)
	}else{
		x[[why]] <- as.flag(x[[why]])
	}
	x[[why]][where] <- as.flag(1)
	class(x[[why]]) <- c('hide',class(x[[why]]))
	x <- shuffle(x,'C')
	x
}

`hidden` <- function(x,...)UseMethod("hidden")

`hidden.data.frame` <- function(x,...){
	hideflags <- x[
		,
		sapply(
			x,
			function(col)all(
				inherits(
					col,
					what=c('hide','flag'),
					which=TRUE
				)
			)
		),
		drop=FALSE
	]
	class(x) <- c('hidden.data.frame',class(x))
	if(!'C' %in% names(x)) return(x[character(0),])
	if(!inherits(x$C,'comment')) {
		warning('C column not of class "comment"')
		return(x[character(0),])
	}
	if(ncol(hideflags) > 0 )if(!(all(x$C==as.logical(rowSums(as.matrix(hideflags))))))warning('mismatch between C column and hide flags')
	y <- x[x$C,]
	y		
}
summary.hidden.data.frame <- function(object,...){
	x <- object
	if('hidden.scope' %in% names(x))stop('hidden.scope is a reserved column name')
	hideflags <- x[
		,
		sapply(
			x,
			function(col)all(
				inherits(
					col,
					what=c('hide','flag'),
					which=TRUE
				)
			)
		),
		drop=FALSE
	]
	if(ncol(hideflags)==0)return(data.frame(ncol=0))
	if(nrow(hideflags)==0)return(data.frame(nrow=0))
	unique <- hideflags[rowSums(hideflags)==1,,drop=FALSE]
	hideflags$hidden.scope <- 'total'
	if(nrow(unique))unique$hidden.scope <- 'unique'
	else unique <- NULL
	molten <- melt(rbind(hideflags,unique),id.var='hidden.scope')
	y <- data.frame(cast(molten,fun=sum))
	rownames(y) <- y$hidden.scope
	y$hidden.scope <- NULL
	y
}



















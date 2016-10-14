`as.flag` <-
function(x,...)UseMethod("as.flag")

f <- as.flag

`as.data.frame.flag` <-
function (x, row.names = NULL, optional = FALSE, ...) 
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

`as.flag.default` <- function(x,...){
	class(x) <- c("flag",class(x))
	x
}

`as.flag.flag` <- function(x,...)x

`c.flag` <- function(...,recursive=FALSE)structure(c(unlist(lapply(list(...), unclass))), class="flag")

`format.flag` <- function(x,...)as.numeric(x)

`print.flag` <- function(x,...){
	print(format(x),...,quote=FALSE)
	invisible(x)
}

`rep.flag` <- function(x,...){
	y <- NextMethod()
	class(y) <- class(x)
	y
}

`[.flag` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

`[[.flag` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

`as.character.flag` <- function(x,...)as.character(format(x))

xtfrm.flag <- function(x)as.numeric(x)


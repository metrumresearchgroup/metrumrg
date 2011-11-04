totitle <-
function(x, strict = FALSE) {
    x <- as.character(x)
    cap <- function(x) paste(toupper(substring(x,1,1)),
                  {x <- substring(x,2); if(strict) tolower(x) else x},
                             sep = "", collapse = " " )
    sapply(strsplit(x, split = " "), cap, USE.NAMES = !is.null(names(x)))
}
titleCase <- function(x,strict=TRUE,...)totitle(x=x,strict=strict,...)

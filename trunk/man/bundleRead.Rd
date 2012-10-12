\name{bundleRead}
\alias{bundleRead}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Read in Data Files from a Directory to List
}
\description{
\code{bundleRead} reads in all files of specified extensions from a directory into a list.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bundleRead(dir, ext, func, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{character vector or list of directories to read files from}
  \item{ext}{character vector of file extensions to grep for }
  \item{func}{list of length \code{ext} of data reading functions to use on the corresponding file extensions}
  \item{\dots}{optional arguments to func}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of (usually of data frames).
}

\author{
Natalie Hsiang
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dir, ext, func, ...) 
{
    func <- ifelse(is.list(func), func, list(func))
    x <- lapply(dir, function(y) list.files(y))
    x <- lapply(x, function(y) {
        keep <- NULL
        for (i in 1:length(ext)) keep <- c(keep, grep(ext[i], 
            y))
        y[order(keep)]
    })
    n <- strsplit(unlist(x), paste(".", ext, sep = ""))
    for (j in 1:length(dir)) x[[j]] <- file.path(dir[j], x[[j]])
    x <- as.list(unlist(x))
    x <- lapply(x, function(y) {
        for (k in 1:length(ext)) if (grepl(ext[k], y)) 
            z <- func[[k]](y)
        return(z)
    })
    names(x) <- n
    return(x)
}
}

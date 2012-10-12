\name{nasum}
\alias{nasum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Count Number of \code{NA} Values in List Items}
%%  ~~function to do ... ~~
}
\description{\code{nasum} returns the number of \code{NA} values in each item of its argument.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nasum(x)
}

\arguments{
  \item{x}{list of numeric, complex, logical, or character objects}
  \item{simplify}{logical whether or not to use \code{sapply} or \code{lapply}
}

\details{
If \code{x} is not a list, it is coerced to \code{list(x)}.
}

\value{
If \code{x} is a list, returns numeric vector of length \code{length(x)}. Otherwise returns numeric vector of length one, the number \code{NA} in \code{x}. 
}
\references{
%% ~put references to the literature/web site here ~
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
nasum(Theoph)
nasum(Theoph,F)
nasum(c(1:10,NA))
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, simplify=TRUE)
{
  if(!is.list(x)) x <- list(x)
  if(simplify) y <- sapply(x,function(y)sum(is.na(y)))
  else y <- lapply(x,function(y)sum(is.na(y)))
  y
}
}
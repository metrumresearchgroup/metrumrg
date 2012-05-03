\name{as.init}
\alias{init}
\alias{initList}
\alias{as.character.init}
\alias{as.character.initList}
\alias{as.init}
\alias{as.init.numeric}
\alias{as.init.init}
\alias{as.initList.character}
\alias{as.initList.list}
\alias{as.initList.numeric}
\alias{as.initList}
\alias{fixed.init}
\alias{fixed.initList}
\alias{fixed}
\alias{fixed<-.init}
\alias{fixed<-.initList}
\alias{fixed<-}
\alias{format.init}
\alias{format.initList}
\alias{print.init}
\alias{print.initList}
\alias{$.init}
\alias{$<-.init}
\alias{[.initList}

\title{Work with Initial Estimates as Objects}
\description{
\code{as.nmctl} and \code{read.nmctl} normally return a list of character vectors as class \code{nmctl}.  Users are free to modify these using conventional techniques for text.  Optionally, the \code{theta} record can be converted to an object of class \code{initList} -- either manually, using \code{as.initList}, or by the 
functions above, passing \code{parse=TRUE}.  \code{initList} is a numbered list of objects of class \code{init}, each of which can be modified using regular R syntax for lists and vectors.  Conveniently, the object number is the same as the THETA index as regarded by NONMEM: e.g. \code{ctl$theta[[3]]} represents THETA3.
}
\usage{
\method{as.init}{numeric}(x = numeric(0), fixed = FALSE, comment = character(0), ...)
\method{as.initList}{character}(x, ...)
\method{as.initList}{numeric}(x, fixed = FALSE, ...)
}
\arguments{
  \item{x}{object of interest}
  \item{fixed}{logical: whether the value should be fixed at the initial estimate.}
  \item{comment}{character: a comment}
  \item{\dots}{ignored}
}
\details{
Class \code{init} represents an initial estimate, e.g. for an element of THETA.  Using the NONMEM help conventions, the estimate is composed of three elements: a lower bound, the estimate itself, and an upper bound; these are named "low", "init", and "up", respectively. They are always present, whether or not displayed. Each may have a numeric value. Furthermore, "low" may be \code{-Inf} (default), "init" may be \code{NA} (if "low" and "up" are finite) and "up" may be \code{Inf} (default). \code{init} may be constructed from numeric vector of length one ('init'), two ('low' and 'init') or three ('low','init','up').

Elements of class \code{init} may be accessed using subset operators (\code{[]}, or either version of the element-select operator (\code{[[]]},\code{$}); assignment is supported for all three cases. Note that a subset of class \code{init} is not itself of that class.

In contrast, a subset of class \code{initList} retains its class.  \code{initList} is a list of objects of class \code{init}. Members may be accessed and assigned using \code{[]} and \code{[[]]}. \code{initList} may be created by parsing a file or by calling \code{as.initList} on a character or numeric vector.

The concept "fixed" is supported.  \code{fixed()} checks whether an \code{init} is fixed, either singly or member-wise for elements of \code{initList}.  \code{fixed() <-} assigns the value \code{TRUE} or \code{FALSE}; for \code{initList}, the supplied value is recycled as necessary to match the length of the primary object. Internally, "fixed" simply means that "low", "init", and "up" are finite and identical.  

Comments are supported. Comments can be multiline, and will be printed following a line representing the initial estimate itself.  When parsing, comments on the same line or following a set of estimates are replicated for all such estimates (perhaps only one).  If an estimate is split across lines, comments for each of those lines are aggregated and associated with the estimate as a whole. Comments are accessed using \code{comment()} and assigned using \code{comment() <- }.

}
\value{
object of class \code{init}, or a numbered set of these of class \code{initList}.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
  \itemize{
  	\item \code{\link{as.nmctl}}
	}
}
\examples{
path <- system.file('example/project/nonmem/ctl/1005.ctl',package='metrumrg')
ctl <- read.nmctl(path,parse=TRUE)
theta <- ctl$theta
theta[[3]]$init
x <- as.initList('(0,3) 2 FIXED (0 .6 1) 10 (-INF,-2.7,0) (37 FIXED) ')
x
fixed(x)
fixed(x) <- TRUE
x
writeLines(as.character(x))
fixed(x) <- FALSE
x
x[[1]]
as.init(1:3)
as.initList(1:3)
as.initList(1:3,fixed=TRUE)
x <- as.initList(1:4)
x
x[1:3]
x[1:2] <- x[3:4]
x
x[[4]]
fixed(x[[4]])
fixed(x[[4]]) <- TRUE
x
x[[4]]$up
x[[4]]$up <- Inf
fixed(x[[4]])
x[[4]][['up']]
x[[4]][c('low','up')]
x <- c(
  ';thetas',
  '10 20; structural',
  '; literature',
  '(',
  '0,;ref A',
  '5,;ref B',
  '10)'
)
writeLines(as.character(as.initList(x)))

}
\keyword{manip}

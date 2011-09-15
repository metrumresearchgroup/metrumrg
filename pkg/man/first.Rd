\name{first}
\alias{first}
\alias{nth}
\alias{last}
\alias{distance}
\alias{before}
\alias{at}
\alias{after}
\alias{only}

\title{Identify Elements Within Cells Meeting Some Instance of a Condition}
\description{
	Within each level of \code{within}, find the \eqn{n^{th}}{n^th} element for which
	\code{where} is \code{TRUE}, and repeat it at all positions in the level.
}
\usage{
nth  (x,where,within,n=1,...)
first(x,where,within,...)
last (x,where,within,...)
only (x,where,within,...)
distance(where,within,n=1,...)
before(  where,within,n=1,...)
at(      where,within,n=1,...)
after(   where,within,n=1,...)
}
\arguments{
  \item{x}{vector (possibly factor)}
  \item{where}{vector, coerced to logical}
  \item{within}{a vector or list of vectors serving as an index}
  \item{n}{an integer giving the instance of interest}
  \item{\dots}{ignored arguments.}

}
\details{
  \code{nth} returns, for each position in \code{x}, the \eqn{n^{th}}{n^th} element of \code{x}, optionally
  limiting candidate elements by \code{where}, and optionally breaking the evaluation across subsets
  as specified by \code{within}.  \code{n} can be 0, returning all \code{NA}, or negative,
  which counts instances  from the end of the vector or subsets.  If \code{n} is NA, all elements are 
  returned.
  
  All of \code{x}, \code{where}, and \code{within} are optional. The \sQuote{scale} of the function
  is set to the longest of the three (member-wise for \code{within} if it is a list).  
  Missing \code{x} is replaced with the subscripts implied by \sQuote{scale}.  
  Missing \code{where} and \code{within} are replaced with TRUE, repeated to length 'scale'.
  No warnings are given if \code{within} has NA, or list members with different lengths.
  
  \code{first} and \code{last} are convenience wrappers for \code{nth} that set \code{n} to 1 
  and -1, respectively.  \code{only} is like \code{first}, but returns NA if a second instance exists.
  
  \code{distance} returns subscripts less the nth subscripts, optionally considering 
  \code{where} and \code{within}. \code{before}, \code{at}, and \code{after} test
  whether distance is less than, equal to, or greater than zero.
}
\value{
	vector of same class as \code{x}, if supplied, else of class integer. Logical for \code{before},
	\code{at}, and \code{after}.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{match}}
	}
}

\examples{
first(1:10)
first(1:10,within=rep(1:2,each=5))
last( 1:10,within=rep(1:2,each=5))
first(1:10,where=0:1,within=rep(1:2,each=5))
test <- data.frame(
	id=c(1,1,1,1,1,2,2,2,2,2),
	time=c(1,2,3,4,5,1,2,3,4,5),
	evid=c(0,1,0,1,0,1,0,1,0,1)
)
test$secondDose  <- with(test,nth(where=evid==1, n=2,within=id))
test$lastDose    <- with(test,nth(where=evid==1,n=-1,within=id))
test$beforeDose2 <- with(test,before(where=evid==1, n=2,within=id))
test$tafd        <- with(test,time - first(time,where=evid==1,within=id))#time after first dose
test
#    id time evid secondDose lastDose beforeDose2 tafd
# 1   1    1    0          4        4        TRUE   -1
# 2   1    2    1          4        4        TRUE    0
# 3   1    3    0          4        4        TRUE    1
# 4   1    4    1          4        4       FALSE    2
# 5   1    5    0          4        4       FALSE    3
# 6   2    1    1          8       10        TRUE    0
# 7   2    2    0          8       10        TRUE    1
# 8   2    3    1          8       10       FALSE    2
# 9   2    4    0          8       10       FALSE    3
# 10  2    5    1          8       10       FALSE    4
only(
	1:9,
	where= c(0,1,0,  1,0,1,  0,0,0),
	within=c(1,1,1,  2,2,2,  3,3,3)
)
#[1]  2  2  2 NA NA NA NA NA NA
}
\keyword{manip}


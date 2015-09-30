\name{reapply}
\alias{reapply}
\title{Apply a Function Across Cells of an Indexed Vector, Giving an Identically-indexed Result}
\description{Like calling \code{tapply} but stretching the result to dimensions of \code{x}.}
\usage{
reapply(x, INDEX, FUN, where, ...)
}
\arguments{
  \item{x}{an atomic object, typically a vector}
  \item{INDEX}{list of factors, each of same length as \code{x}}
  \item{FUN}{a function that accepts a vector and returns a vector}
  \item{where}{logical, restricting the domain of x}
  \item{\dots}{optional arguments to \code{FUN}}
}
\details{
The function \code{tapply} applies \code{FUN} to each cell of a vector, as 
specified by levels of \code{INDEX}. \code{reapply} repeats that result as necessary
to match the number of input elements per cell, and restores the order to that
of the original index.  Regardless of the length of the value of \code{FUN}, the
length of the value of reapply is always identical to that of \code{x}. 
If \code{where} is specified, elements in x corresponding to \code{FALSE} are
not passed to \code{FUN}.  Argument \code{where} is recycled as necessary and coerced
to logical, with NA set to \code{TRUE}.

\code{reapply} gives a result similar to \code{stats::ave}. However, 
\code{ave} gives a warning if the input for a cell is not a multiple of the output.  
Also, \code{reapply} has a named argument for the index
variables, and passes dots to \code{fun}; for \code{ave}, dots represent the indices,
and any extra arguments to \code{fun} need to be hard-coded, e.g. in an anonymous
function (see examples for \code{ave}). Finally, \code{reapply} returns NA for any 
element where some member of INDEX is NA, whereas \code{ave} applies \code{FUN} to 
these elements in isolation.
}
\value{an atomic object, typically a vector}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\note{
Caution!  If \code{FUN} is isometric (output has same length as input, e.g. \code{cumsum} but not \code{mean}) then
use of \code{where} may give surprising results.  If \code{where} is ever \code{FALSE},
then the input to \code{FUN} will be necessarily shorter than the required output,
and recycling will be in effect.
}
\seealso{
	\itemize{
		\item \code{\link{tapply}}
		\item \code{\link{lapply}}
		\item \code{\link{rep}}
		\item \code{\link{ave}}
	}
}\examples{
theoph <- Theoph[c(1,12,2,13,3,14),]
theoph$avg <- with(theoph,reapply(conc,Subject,mean))
theoph$avg2 <- with(theoph,reapply(conc,Subject,mean,where=Time!=0,na.rm=TRUE))
theoph$sum <- with(theoph,reapply(conc,Subject,cumsum))
theoph
Theoph <- Theoph[Theoph$Subject \%in\% c(1,2),]
Theoph <- Theoph[distance(within=Theoph$Subject) < 4,]
Theoph$Subject <- as.character(Theoph$Subject)
Theoph$where <- TRUE
Theoph <- within(Theoph, where[Subject == 2 & conc == 0] <- FALSE)
Theoph <- rbind(Theoph, aug(Theoph[1,],Subject = NA))
Theoph$misc <- reapply(Theoph$conc,INDEX=Theoph[,c('Subject','Dose')],
where=Theoph$where,FUN= `[`,1:3)
Theoph
stopifnot(class(reapply(c('foo','bar'),INDEX=c(1,1),FUN=function(x)x=='foo'))=='logical')
}
\keyword{manip}


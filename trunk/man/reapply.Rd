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
  \item{FUN}{the function to be applied. In the case of functions like \code{+}, \code{\%*\%}, etc., the function name must be quoted.}
  \item{where}{logical, restricting the domain of x}
  \item{\dots}{optional arguments to \code{FUN}}
}
\details{
The function \code{tapply} applies \code{FUN} to each cell of a vector, as 
specified by levels of \code{INDEX}. \code{reapply} repeats that result as necessary
to match the number of input elements per cell, and restores the order to that
of the original index.  Regardless of the length of the value of \code{FUN}, the
length of the value of reapply is always identical to that of \code{x}. \code{where},
if specified, pre-processes x by setting non-indicated elements to NA (where is 
recycled as necessary); be sure FUN handles NA correctly, e.g. by passing 
\code{na.rm=TRUE}.

\code{reapply} gives a result almost identical to \code{stats::ave}. However, 
\code{reapply} does not require a strict "multiple" relationship between the 
input and output of \code{fun}.  \code{reapply} has a named argument for the index
variables, and passes dots to \code{fun}; for \code{ave}, dots represent the indices,
and any extra arguments to \code{fun} need to be hard-coded, e.g. in an anonymous
function (see examples for \code{ave}).
}
\value{an atomic object, typically a vector}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\note{
	\code{NA} is returned whererever a component of \code{INDEX} is \code{NA}.
}
\seealso{
	\itemize{
		\item \code{\link{tapply}}
	}
}\examples{
theoph <- Theoph[c(1,12,2,13,3,14),]
theoph$avg <- with(theoph,reapply(conc,Subject,mean))
theoph$sum <- with(theoph,reapply(conc,Subject,cumsum))
theoph
}
\keyword{manip}


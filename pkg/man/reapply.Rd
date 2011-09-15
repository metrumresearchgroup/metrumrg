\name{reapply}
\alias{reapply}
\title{Apply a Function Across Cells of an Indexed Vector, Giving an Identically-indexed Result}
\description{Like calling \code{tapply} but stretching the result to dimensions of \code{x}.}
\usage{
reapply(x, INDEX, FUN, ...)
}
\arguments{
  \item{x}{an atomic object, typically a vector}
  \item{INDEX}{list of factors, each of same length as \code{x}}
  \item{FUN}{the function to be applied. In the case of functions like \code{+}, \code{\%*\%}, etc., the function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}}
}
\details{
The function \code{tapply} applies \code{FUN} to each cell of a vector, as 
specified by levels of \code{INDEX}. \code{reapply} repeats that result as necessary
to match the number of input elements per cell, and restores the order to that
of the original index.  Regardless of the length of the value of \code{FUN}, the
length of the value of reapply is always identical to that of \code{x}.
}
\value{an atomic object, typically a vector}
\references{\url{http://mifuns.googlecode.com}}
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


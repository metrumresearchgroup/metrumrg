\name{sets}
\alias{as.vector.keyed}
\alias{\%+\%}
\alias{\%&\%}
\alias{\%-\%}
\alias{\%u\%}
\alias{\%n\%}
\alias{\%+\%.default}
\alias{\%&\%.default}
\alias{\%-\%.default}

\title{Binary Operators for Set Relations}
\description{These functions test set relations using binary operators.  See details.}
\usage{
\method{\%+\%}{default}(x, y)
\method{\%&\%}{default}(x, y)
\method{\%-\%}{default}(x, y)
\method{as.vector}{keyed}(x,mode='any')
}
\arguments{
  \item{x}{left operand}
  \item{y}{right operand}
  \item{mode}{currently ignored}
 }
\details{
	The function \code{\%+\%} and its alias \code{\%u\%} give set union, with obvious mnemonic significance.
	\code{\%&\%} gives set intersection; mnemonically, elements present in x \emph{and} y.
	\code{n} looks like an inverted \code{u} in many fonts, so \code{\%n\%} gives set intersection
	in something more like classical notation.  \code{\%-\%} is suggestive of set difference
	both in arithmetic and classical notation.
	
	These functions call \code{union}, \code{intersect}, and \code{setdiff}, which 
	coerce their arguments to vector.  For convenience, \code{as.vector.keyed} is defined
	to return just the names of a keyed data frame -- frequently these are subject to set manipulations.
}
\value{
vector
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{union}}
		\item \code{\link{intersect}}
		\item \code{\link{setdiff}}
	}
}
\examples{
c('a','b','c') \%u\% c('c','d','e')
c('a','b','c') \%n\% c('c','d','e')
c('a','b','c') \%-\% c('c','d','e')
as.vector(as.keyed(Theoph,c('Subject', 'Time')))
}
\keyword{manip}

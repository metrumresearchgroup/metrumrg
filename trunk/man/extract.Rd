\name{extract}
\alias{extract}

\title{Extract a Parenthetically-grouped Substring of a Regular Expression}
\description{
Given a regular expression with parenthetically-grouped substrings, return or drop
the substring of interest.
}
\usage{
extract(x, pattern, group = 0, invert = FALSE, ...)
}

\arguments{
  \item{x}{character}
  \item{pattern}{passed to \code{\link{regexec}}}
  \item{group}{one less than the index used by \code{\link{regexec}}}
  \item{invert}{drop the matched text, returning everything else}
  \item{\dots}{ignored}
}
\details{
\code{\link{regexec}} determines all the numbered matches corresponding to 
 sets of parentheses.  \code{extract} uses these to locate and return particular
 numbered matches, or to delete them, returning what remains. \code{group = 0} 
 refers to the entire match (first element of each regexec result).  Non-matches 
 give an empty string; matching NA results in NA.
}
\value{
character
}
\references{\url{http://metrumrg.googlecode.com}}

\author{ Tim Bergsma }
\seealso{
	\itemize{

		\item \code{\link{regexec}}
		\item \code{\link{guidetext}}
	}
}
\examples{
x <- c(' kg ',' //a/A//b/B// ',NA,' kg [ 4 , 8.2 ]','(,1.025e-6]','(,] ')
pattern <- '((\\\\(|\\\\[) *([-+eE.0-9]*) *, *([-+eE.0-9]*) *(\\\\)|\\\\])) *$'
extract(x,pattern,group=5)	
}
\keyword{manip}

\name{metaMerge}
\alias{metaMerge}
\alias{metaMerge.list}
\alias{metaMerge.character}
\alias{metaMerge.data.frame}
\alias{metaMerge.default}
\alias{metaMerge.NULL}
\title{Recursively Merge Multiple Tables}
\description{
  Perform an n-way merge, given multiple objects or file names.
}
\usage{
	\method{metaMerge}{list}(x,all=TRUE,...)
	\method{metaMerge}{character}(x,import=read.table,all=TRUE,...)
	\method{metaMerge}{default}(x,y,all=TRUE,...)
	\method{metaMerge}{data.frame}(x,y,all=TRUE,...)
	\method{metaMerge}{NULL}(x,y,all=TRUE,...)
}
\arguments{
  \item{x}{a list of objects, e.g. data.frames, or a character vector of file names}
  \item{y}{a second object}
  \item{all}{passed to \code{merge}}
  \item{import}{a function for reading files}
  \item{\dots}{passed to \code{import}}
}
\details{
	If \code{x} is a character vector, it is presumed to be file names.  An 
	error is returned if any file does not exist.  Otherwise, files are acquired
	using \code{import} (after a call to \code{match.fun}). The resulting list
	of objects is merged.
	
	If \code{x} is an empty list, it is returned. If of length one, the first 
	element is returned.  Otherwise, the first element is merged with the second,
	and the result merged with the third (if present), and so on.  Merges are
	full or outer merges; i.e. \code{all=TRUE}.
	
	The default method simply calls \code{merge} and is present for methodological reasons.
}
\note{
	In most cases, \code{merge} is eventually called.  Since \code{merge} is generic, 
	what happens next depends on the class of the arguments. For example, a merge method
	has been defined for class \code{nm}, which may give unexpected results.
}
\value{
	object like the elements of \code{x}, or given by \code{import}
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{merge}}
	}
}
\examples{
#a nonsense example
head(metaMerge(list(Theoph,BOD)))
}
\keyword{manip}


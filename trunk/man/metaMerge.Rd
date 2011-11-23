\name{metaMerge}
\alias{metaMerge}
\alias{metaMerge.list}
\alias{metaMerge.character}
\alias{metaMerge.default}
\title{Recursively Merge Multiple Tables}
\description{
  Perform an n-way merge, given multiple objects or file names.
}
\usage{
	\method{metaMerge}{list}(x,groom=as.best,...)
	\method{metaMerge}{character}(x,import=read.table,groom=as.best,...)
	\method{metaMerge}{default}(x,y,...)
}
\arguments{
  \item{x}{a list of objects, e.g. data.frames, or a character vector of file names}
  \item{y}{a second object}
  \item{import}{a function for reading files}
  \item{groom}{a function for preparing objects prior to merge}
  \item{\dots}{passed to \code{groom} and \code{import}}
}
\details{
	If \code{x} is a character vector, it is presumed to be file names.  An 
	error is returned if any file does not exist.  Else, files are aquired
	using \code{import} (after a call to \code{match.fun}). The resulting list
	of objects is merged.
	
	If \code{x} is an empty list, it is returned. Otherwise, each element is
	transformed using using \code{groom} (after a call to \code{match.fun}).
	The first object is merged with the merge of any remaining objects.
	
	The default method simply calls \code{merge} and is present for methodological reasons.
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


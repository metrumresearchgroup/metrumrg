\name{merge}
\alias{merge.list}
\alias{merge.character}
\title{Recursively Merge Multiple Tables}
\description{
  Perform an n-way merge, given multiple objects or file names
}
\usage{
	\method{merge}{list}(x,groom=as.best,...)
	\method{merge}{character}(x,import=read.table,groom=as.best,...)
}
\arguments{
  \item{x}{a list of objects, e.g. data.frames, or a character vector of file names}
  \item{import}{a function for reading files}
  \item{groom}{a function for preparing objects prior to merge}
}
\details{
	If \code{x} is a character vector, it is presumed to be file names.  An 
	error is returned if any file does not exist.  Else, files are aquired
	using \code{import} (after a call to \code{match.fun}). The resulting list
	of objects is merged.
	
	If \code{x} is an empty list, it is returned. Otherwise, each element is
	transformed using using \code{groom} (after a call to \code{match.fun}).
	The first object is merged with the merge of any remaining objects.
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
head(merge(list(Theoph,BOD)))
}
\keyword{manip}


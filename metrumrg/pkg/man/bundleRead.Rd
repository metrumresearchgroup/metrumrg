\name{bundleRead}
\alias{bundleRead}
\title{
Read Data Files from a Directory into a List
}
\description{
\code{bundleRead} Reads all files of specified extensions from a directory into a list.
}
\usage{
bundleRead(dir, ext, func, ...)
}
\arguments{
  \item{dir}{character vector or list of directories from which to read files}
  \item{ext}{character vector of file extensions to seek }
  \item{func}{list of length \code{ext} of "read" functions to use on the corresponding file extensions}
  \item{\dots}{optional arguments to func}
}
\value{
list (usually, of data frames)
}
\references{\url{http://metrumrg.googlecode.com}}
\author{
Natalie Hsiang
}
\seealso{
	\itemize{
		\item \code{\link{lapply}}
	}
}
\keyword{manip}


\name{purge.files}
\alias{purge.files}

\title{Purge Files}
\description{
	If \code{dir} is a directory, remove files matching the pattern.
}
\usage{
purge.files(pattern, dir = '.')
}

\arguments{
  \item{pattern}{a regular expression}
  \item{dir}{a directory path}
}
\value{used for side effects}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{purge.dir}}
	}
}
\keyword{manip}


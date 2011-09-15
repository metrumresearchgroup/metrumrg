\name{purge.dir}
\alias{purge.dir}

\title{Purge a Directory}
\description{
 Purge a directory, perhaps nicely.
}
\usage{
purge.dir(dir, nice = FALSE)
}

\arguments{
  \item{dir}{a directory}
  \item{nice}{whether to purge system files}
}
\details{
	If \code{nice==TRUE}, system files (as defined for \code{all.files} in \code{dir}) are not removed.
}
\value{used for side effects.}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runNonmem}}
	}
}
\keyword{manip}


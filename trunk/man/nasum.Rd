\name{nasum}
\alias{nasum}
\title{Count Number of \code{NA} Values in List Items}
}
\description{\code{nasum} Count the number of \code{NA} values in each element of a list-like object.
}
\usage{
nasum(x)
}

\arguments{
  \item{x}{list of vector-like objects; e.g. a data frame}
  \item{simplify}{logical: coerce to vector result if possible}
}

\details{
	Arguments are passed to \code{simplify}, along with a function that sums instances of \code{NA}.
}

\value{
numeric or list
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Natalie Hsiang}
\seealso{
	\itemize{
		\item \code{\link{is.na}}
	}
}
\examples{
nasum(Theoph)
}
\keyword{manip}

\name{breaks}
\alias{breaks}
\title{Identify Boundaries Between Sets of Elements}
\description{Identify boundaries between sets elements.}
\usage{
breaks(x, ...)
}
\arguments{
  \item{x}{vector}
  \item{\dots}{ignored}
}
\details{A vector of length n has at most n-1 divisions
between elements.  If the elements are taken in runs of 
repeated elements, a set of divisions <= n-1 may be identified.
This function returns a zero for each between-element position
if the bounding elements are identical, and a one for each
between-element position if the bounding elements differ.
Used for placing lines between sets of rows or columns in a table.
}
\value{integer}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{tabular.data.frame}}
	}
}
\examples{
breaks(c(1,1,1,2,2,2,3,3,3))
}
\keyword{manip}


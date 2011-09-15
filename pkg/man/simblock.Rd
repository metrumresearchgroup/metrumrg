\name{simblock}
\alias{simblock}
\title{Generate Random Effect Deviates for Simulation}
\description{
  Generate random effect values given the number of simulations, degrees of freedom, and
  a variance-covariance matrix (block).
}
\usage{
simblock(n, df, cov)
}
\arguments{
  \item{n}{the number of simulations that will be performed}
  \item{df}{vector of degrees of freedom of block elements}
  \item{cov}{variance-covariance matrix}
}
\details{
  \code{df} and \code{cov} should have the same length.
}
\value{
 matrix, with one row per simulation and one column per element in the corresponding
triangular version of \code{cov}. 
}
\references{\url{http://mifuns.googlecode.com}}
\author{written by Leonid Gibiansky; modified by Tim Bergsma.}
\seealso{
	\itemize{
		\item \code{\link{simpar}}
	}
}
\keyword{manip}

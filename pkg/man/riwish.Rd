\name{riwish}
\alias{riwish}
\title{Generate Inverse Wishart Distribution}
\description{
 Generates an inverse Wishart distribution for a variance of a random effect.
 The functions requires the specification of degrees of freedom, scale matrix, 
 and precision for each random effect.
}
\usage{
riwish(s, df, prec)
}
\arguments{
  \item{s}{scale matrix for inverse Wishart distribution}
  \item{df}{degrees of freedom}
  \item{prec}{presision matrix: the mean of \code{riwish} is proportional to \code{prec}}
}
\value{
 The variance of the random effect can be drawn from the returned inverse Wishart distribution.
}
\references{\url{http://mifuns.googlecode.com}}
\author{written by Leonid Gibiansky; modified by Tim Bergsma.}
\keyword{manip}

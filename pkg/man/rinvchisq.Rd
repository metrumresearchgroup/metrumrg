\name{rinvchisq}
\alias{rinvchisq}
\title{Generate Inverse Chi-square Distribution}
\description{
	Generate inverse chi-square distribution given \code{n}, \code{df}, and covariance matrix
}
\usage{
rinvchisq(n, df, cov)
}
\arguments{
  \item{n}{the number of values to return}
  \item{df}{the degrees of freedom for the distribution. This value reflects the 
  uncertainity in the distribution.  A reasonable starting point would be a value 
  between the number of subjects and the total number of observations used 
  to obtain the estimates of the variance-covariance matrix.}
  \item{cov}{a single parameter}
}
\value{
 The \code{rinvchisq} function returns \code{n} parameter deviates for use as simulation parameters.
}
\references{\url{http://mifuns.googlecode.com}}
\author{written by Leonid Gibianski; modified by Tim Bergsma.}
\keyword{utilities}

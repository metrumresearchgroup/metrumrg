\name{cv}
\alias{cvLognormal}
\alias{cvNormal}
\alias{cv}
\title{
Compute Coefficient of Variation for Normal and Log-Normal Data.
}
\description{
The CV for normally distributed data can be calculated as a function
of variance and mean.

The CV for log-normally distributed data can be calculated as a function
of the variance of the log-transformed data.  See references below.
}
\usage{
cvNormal(var,mean, ...)
cvLognormal(var, ...)
}
\arguments{
  \item{var}{variance of the normal data or log-transformed lognormal data}
  \item{mean}{mean of the normal data}
  \item{\dots}{ignored}
}
\value{numeric}
\references{
\url{http://en.wikipedia.org/wiki/Coefficient_of_variation}

\url{http://en.wikipedia.org/wiki/Log-normal_distribution#Coefficient_of_variation}

\url{http://tolstoy.newcastle.edu.au/R/help/05/12/16710.html}
}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{percent}}
	}
}
\examples{
norm <- rnorm(10000,mean=10,sd=5)
lnrm <- rlnorm(10000,meanlog=10,sdlog=.3)
cvNormal(sd(norm)^2,mean(norm))
cvLognormal(sd(log(lnrm))^2)
}
\keyword{manip}


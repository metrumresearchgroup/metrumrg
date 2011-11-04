\name{cvLognormal}
\alias{cvLognormal}
\alias{cvNormal}
\title{
Compute Coefficient of Variation for Normal and Log-Normal Data.~~
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

\url{http://en.wikipedia.org/wiki/Log-normal_distribution#Coefficient_of_variation}
}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{percent}}
	}
}
\examples{
cvLognormal	
}
\keyword{manip}


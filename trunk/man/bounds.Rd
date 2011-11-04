\name{bounds}
\alias{bounds}
\alias{upper}
\alias{lower}
\title{
Calculate Normal Quantiles at Boundaries of Probability Intervals~~
}
\description{
	\code{qnorm} expects scalar means and standard deviations, 
	but a vector of probablities. In contrast, \code{lower} and 
	\code{upper} are wrappers for \code{qnorm} that accept
	equal-length vectors for mean and standard deviation, but 
	scalar probabilities. Probability can be supplied directly
	or calculated as the bound of a symmetric interval. \code{bounds}
	pairs \code{lower} and \code{upper} using suitable notation.~~
}
\usage{
lower(x, sd, interval = 0.95, prob = 0.5 * (1 - interval), ...)
upper(x, sd, interval = 0.95, prob = 0.5 * (1 + interval), ...)
bounds(x, sd, interval = 0.95, lower = 0.5 * (1 - interval), upper = 0.5 * (1 + interval), left = "(", right = ")", sep = ",", digits = 3, ...)
}
\arguments{
  \item{x}{means}
  \item{sd}{standard deviations, of same length as \code{x}}
  \item{prob}{probability, defaulting to the bounds of a symmetric interval}
  \item{interval}{width of a probability interval: e.g. confidence interval}
  \item{lower}{probability for the lower bound}
  \item{upper}{probability for the upper bound}
  \item{left}{character(s) to delimit the lower bound}
  \item{right}{character(s) to delimit the upper bound}
  \item{sep}{characters to delimit lower and upper quantiles}
  \item{digits}{number of significant digits: passed to \code{signif}}
  \item{\dots}{other arguments, passed to \code{qnorm}}
}
\details{
	\code{interval} is a convenient~way to set upper and lower probabilities,
	and is ignored if these are specified directly.  \code{left}
	and \code{right} are passed to \code{paste}, and so will be 
	repeated as necessary to match the length of \code{x}.~
}
\value{
	\code{lower} and \code{upper} return numeric.  \code{bounds}
	returns character.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{qnorm}}
	}
}
\examples{
bounds(c(0,0,0),sd=c(.5,1,2))	
}
\keyword{manip}


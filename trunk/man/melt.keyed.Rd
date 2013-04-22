\name{melt.keyed}
\alias{melt.keyed}
\title{Melt a Keyed Data Frame}
\description{
Melt an object with class c('keyed','data.frame').  See also \code{\link{melt.data.frame}}.
}
\usage{
\method{melt}{keyed}(
  data, 
  id.vars=key(data), 
  measure.vars, 
  variable_name = "variable", 
  na.rm = FALSE, 
  ...
)
}

\arguments{
  \item{data}{passed to \code{melt}}
  \item{id.vars}{passed to \code{melt}}
  \item{measure.vars}{passed to \code{melt}}
  \item{variable_name}{passed to \code{melt}}
  \item{na.rm}{passed to \code{melt}}
  \item{\dots}{passed to \code{melt}}
}
\details{
The package \pkg{reshape} declares the S3 generic \code{melt}, and declares a {data.frame} method for it.  The \code{keyed} method is similar, but returns a \code{keyed} object. The key consists of all columns except \code{value}.  
}
\value{
An object with class c('keyed','data.frame')
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
  \itemize{
		\item \code{\link{cast,keyed-method}}
	}
}
\examples{
time <- rep(2,8)
start <- cumsum(time)
start <- as.mTime(as.second(as.hour(start)))
stop <- start + as.second(as.minute(30))
x <- data.frame(
  subj = rep(c('a','b'), each=4),
  period = 1:4,
  start=start,
  stop=stop
)
x <- as.keyed(x, c('subj','period'))
x 
summary(x)
y <- melt(x)
y
summary(y)
cast(y)
}

\keyword{ manip }

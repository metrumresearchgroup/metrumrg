\name{closers}
\alias{closers}
\title{Calculate Closing Text for Nested Script Levels}
\description{Detect target locations for subscript and superscipt end-tags, and place the specified substitutes.}
\usage{
closers(x, sub, sup, ...)
}
\arguments{
  \item{x}{character}
  \item{sub}{character}
  \item{sup}{character}
  \item{\dots}{ignored}
}
\details{Used by \code{wikiparse}.
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{wikiparse}}
	}
}
\keyword{manip}

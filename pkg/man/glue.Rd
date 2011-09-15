\name{glue}
\alias{glue}
\title{Paste Items with No Space}
\description{
	The default separator for \code{paste} is a space.  Pasting
	with an empty string as separator seems so ubiquitous as 
	to merit its own function. \code{glue} is just a wrapper for 
	\code{paste} with an empty string separator.
}
\usage{
glue(...,sep='',collapse=NULL)
}
\arguments{
  \item{\dots}{one or more R objects, to be converted to character vectors.}
  \item{sep}{a character string to separate the terms.}
  \item{collapse}{an optional character string to separate the results.}
 }
\details{
	All arguments are passed unmodified to \code{paste}  See the related help for detials.
}
\value{
character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{paste}}
	}
}
\examples{
glue('cross','road')
}
\keyword{manip}


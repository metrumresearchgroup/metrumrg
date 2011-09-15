\name{command}
\alias{command}
\title{Format a Latex Command}
\description{Format a latex command.}
\usage{
command(x, options = NULL, args = NULL, depth = 0)
}
\arguments{
  \item{x}{length one character}
  \item{options}{vector or list}
  \item{args}{vector or list}
  \item{depth}{integer}
}
\details{\code{x} is formated as a latex command, with
the options (possibly named) inserted in square brackets,
and the arguments each enclosed in curly braces.  \code{depth} 
spaces are added to the left end of the string.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{wrap}}
			\item \code{\link{ltable.data.frame}}
			\item \code{\link{spaces}}
	}
}
\examples{
command('caption',options='short',args='long')
}
\keyword{manip}


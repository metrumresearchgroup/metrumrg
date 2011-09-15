\name{wrap}
\alias{wrap}
\title{Wrap Text in a Latex Environment}
\description{Wrap text in a latex environment.}
\usage{
wrap(x, environment, options = NULL, args = NULL, depth = 0)
}
\arguments{
  \item{x}{character}
  \item{environment}{name of environment}
  \item{options}{list or vector}
  \item{args}{list or vector}
  \item{depth}{integer: extra spaces on the left.}
}
\details{\code{x} is wrapped in the specified environment,
with options and aruments formatted appropriately.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{command}}
			\item \code{\link{latex.args}}
			\item \code{\link{latex.options}}
			\item \code{\link{spaces}}
			\item \code{\link{ltable.data.frame}}
	}
}
\examples{
wrap('Hello','center')
}
\keyword{manip}


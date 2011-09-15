\name{is.latex.token}
\alias{is.latex.token}
\title{Determine if String is Valid Latex Command or Environment}
\description{Determine if string is valid latex command or environment name.}
\usage{
is.latex.token(x, ...)
}
\arguments{
  \item{x}{character}
  \item{\dots}{ignored}
}
\details{Latex commands (and presumably, environment names)
must consist entirely of upper or lower case letters, or must
be a single non-letter.}
\value{logical}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{is.alpha}}
			\item \code{\link{is.one.nonalpha}}
	}
}
\examples{
is.latex.token('2','word','word2')
}
\keyword{manip}


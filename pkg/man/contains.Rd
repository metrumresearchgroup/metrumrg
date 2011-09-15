\name{contains}
\alias{contains}
\title{Test a Character Vector for Occurences of a Pattern}
\description{Test a character vector for occurences of a pattern.}
 \usage{
contains(pattern, text, ...)
}
\arguments{
  \item{pattern}{regular expression}
  \item{text}{character vector}
  \item{\dots}{extra arguments to \code{regexpr}}
}
\details{
	Each element of \code{text} is tested for \code{pattern}, and returns \code{TRUE} if \code{pattern} matches.
}
\value{
logical vector as long as \code{text}
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{regexpr}}
	}
}
\examples{
contains(
  'glu',
  c(
    'glucose',
    'glucuronidase',
    'glucogen',
    'fibrogen'
  )
)
}
\keyword{manip}

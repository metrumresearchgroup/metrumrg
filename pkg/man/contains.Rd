\name{contains}
\alias{contains}
\alias{\%contains\%}
\title{Test a Character Vector for Occurences of a Pattern}
\description{Test a character vector for occurences of a pattern.}
 \usage{
contains(pattern, text, ...)
x \%contains\% y
}
\arguments{
  \item{pattern}{regular expression}
  \item{text}{character vector}
  \item{\dots}{extra arguments to \code{regexpr}}
  \item{x}{vector}
  \item{y}{pattern to seek in \code{x}}
}
\details{
	Each element of \code{text} is tested for \code{pattern}, and returns \code{TRUE} if \code{pattern} matches.
	The binary operator version implements a more natural syntax.
}
\value{
logical vector as long as \code{text}
}
\references{\url{http://metrumrg.r-forge.r-project.org}}
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
letters %contains% 'a'
}
\keyword{manip}

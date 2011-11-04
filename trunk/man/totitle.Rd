\name{totitle}
\alias{totitle}
\alias{titleCase}
\title{
Convert Text to Title Case~~
}
\description{
Convert text to title case.  I.e., capitalize each word.  If strict
is TRUE, force subsequent letters in each word to be lower case.~
}
\usage{
totitle(x, strict = FALSE)
titleCase(x, strict = TRUE,...)
}
\arguments{
  \item{x}{
character~~
}
  \item{strict}{
Shall non-first letters of words be coerced to lower case?~~
}
}
\details{
	The code for \code{totitle} is borrowed from the R help for \code{toupper} (capword). ~~
}
\value{
character
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{toupper}}
		\item \code{\link{tolower}}
	}
}
\examples{
totitle('THETA1')	
}
\keyword{manip}


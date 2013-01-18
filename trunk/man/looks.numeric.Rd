\name{looks.numeric}
\alias{looks.numeric}
\title{Test Whether a Vector Looks Like a Numeric When Printed}

\description{
	Some vectors are stored in R as numerics even though their display
	properties include characters.  \code{looks.numeric}
	is analogous to \code{is.numeric}, but tests for display type rather than 
	storage type.
}
\usage{
looks.numeric(x)
}
\arguments{
  \item{x}{vector}
}
\details{The function tests whether all elements in \code{x} follow this pattern
(after stripping spaces): optional + or - , followed by optional string of digits,
optional decimal character (\code{getOption('OutDec')}), and optional string of digits.
Note that values in scientific notation do not fit this pattern.}
\value{logical}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{is.numeric}}
			\item \code{\link{as.best}}
	}
}
\examples{
looks.numeric(c(1,NA))
looks.numeric(c(1,Inf))
looks.numeric(NULL)
looks.numeric(c(1,NULL))
looks.numeric(c(1,NaN))
looks.numeric(as.mDateTime(0))
looks.numeric(1:10)
looks.numeric(letters)
looks.numeric(1e-5)
looks.numeric(as.comment(FALSE))
looks.numeric(as.comment(TRUE))
looks.numeric(as.comment(c(TRUE,FALSE)))
looks.numeric(numeric(0))
looks.numeric('+123.456')
looks.numeric('0.')
looks.numeric('.')
looks.numeric('.0')

}
\keyword{manip}


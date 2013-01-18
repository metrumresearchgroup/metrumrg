\name{looks.numeric}
\alias{looks.numeric}
\title{Test Whether a Vector Looks Like a Numeric When Printed}

\description{
	Some vectors are stored in R as numerics even though their display
	properties include characters.  \code{looks.numeric}
	is analogous to \code{is.numeric}, but tests not for storage type, but rather for 
	display type.
}
\usage{
looks.numeric(x)
}
\arguments{
  \item{x}{vector}
}
\details{Verifies that each element in \code{x} contains only 
upper or lower case letters.}
\value{logical}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{is.numeric}}
			\item \code{\link{as.best}}
	}
}
\examples{
looks.numeric(NA)
looks.numeric(Inf)
looks.numeric(NULL)
looks.numeric(NaN)
looks.numeric(as.mDateTime(0))
looks.numeric(1:10)
looks.numeric(letters)
looks.numeric(1e-5)
looks.numeric(as.comment(FALSE))
looks.numeric(as.comment(TRUE))
}
\keyword{manip}


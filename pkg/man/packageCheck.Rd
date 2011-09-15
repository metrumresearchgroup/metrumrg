\name{packageCheck}
\alias{packageCheck}
\title{Load a Package and Run Package-level Examples for Testing Purposes}
\description{
\code{packageCheck} attempts to load the specified package and to run \code{example} with
  the package name as an argument.
}
\usage{
packageCheck(x,lib.loc=NULL)
}
\arguments{
  \item{x}{scalar character, the name of a single package}
  \item{lib.loc}{the library to check, passed to \code{library}}
}
\details{
Many packages do not have package-level examples; the call to \code{example} in 
 such cases does nothing (warnings are suppressed).
}
\value{
an scalar character string: zero if the package does not load or if \code{example} 
  generates an error; otherwise, the package version.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{accept}}
		\item \code{\link{acceptance}}
		\item \code{\link{library}}
	}
}
\keyword{manip}


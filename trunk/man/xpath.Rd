\name{xpath}
\alias{xpath}

\title{Extract Value of an xpath Expression from and XML File}
\description{
 Given an xpath expression, find corresponding values in an XML tree stored as a file.
}
\usage{
xpath(x,file)
}

\arguments{
  \item{x}{a valid xpath expression}
  \item{file}{(path and) name of a file}
}
\details{
	This relies on the XML package.  Somewhat experimental.
}
\value{same as \code{xpathSapply}}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{xpathSApply}}
	}
}
\keyword{manip}


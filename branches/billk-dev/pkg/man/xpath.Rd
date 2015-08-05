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
\value{same as \code{xpathSApply}}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{xpathSApply}}
	}
}
\examples{
	filename <- 'example/project/nonmem/1005/1005.xml'
	xpath('//nm:theta/nm:val', system.file(filename,package='metrumrg'))
	xpath('//nm:monitor/nm:obj', system.file(filename,package='metrumrg'))
	xpath('//nm:covariance/nm:row', system.file(filename,package='metrumrg'))

}
\keyword{manip}


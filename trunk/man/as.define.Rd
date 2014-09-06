\name{as.define}
\alias{as.define}
\alias{as.define.spec}
\title{Work with Data Definitions}
\description{
For an existing \code{spec} object, \code{as.define} creates a \code{define} object.
For a \code{define} object, methods cascade:  \code{as.pdf} calls \code{as.document}
which calls \code{tabular}.  Any of these may be called directly.
}
\usage{
\method{as.define}{spec}(x,sep = ' = ',collapse = '; ',escape = c('_','\%','$'),...)
}
\arguments{
  \item{x}{a spec object}
  \item{sep}{separates codes from respective decodes where given}
  \item{collapse}{separates code/decode pairs where given}
  \item{escape}{values to escape for proper latex formatting}
  \item{\dots}{extra arguments, ignored}
}
\value{
object of class `define`
}
\note{ A newline character in \code{collapse} may be combined with fixed width column(s)
	in \code{as.document} to good effect (wrapping of long code/decode lists).
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\code{\link{specification}}, 
	\code{\link{as.spec}}
	\code{\link{tabular.define}}
	\code{\link{as.document.define}}
	\code{\link{as.pdf.define}}
}
\examples{
example(`%matches%`)
}
\keyword{manip}


\name{specification}
\alias{specification}
\alias{specification.default}
\alias{specification.comment}
\alias{specification.data.frame}

\title{Create a Data Specification Corresponding to a Particular Data Set}
\description{
	\code{as.spec} works with objects that already look like metadata.  In 
	contrast, \code{specification} draws up draft metadata, given a candidate.
	The data.frame method makes the generic call on each of its members before
	proceeding, thus giving programmers a chance to provide preparatory methods.
}
\usage{
	\method{specification}{default}(x, ...)
	\method{specification}{comment}(x, ...)
	\method{specification}{data.frame}(x, tol = 10, sep = '/',...)
}

\arguments{
  \item{x}{object of dispatch}
  \item{tol}{consider an integer to be factor-like if it has this or fewer unique values.}
  \item{sep}{use this separator to \code{\link{encode}} factor-like objects.}
  \item{\dots}{passed to other functions}
}
\details{
\code{specification} pre-processes each column, and then proceeds with generic 
documentation (see \code{\link{as.spec}}).  It tries to guess among the standard types: 
character, numeric, integer,
datetime, providing range and level information as appropriate. By default, labels
are the same as the column names, unless the columns have a label attribute. Labels for 
the levels of factor-like objects (true factors, and other vectors with few unique values)
are by default the same as the values themselves. Units for numerics are not guessed.
Typically one refines the resulting spec to more closely specify data characteristics.  
It should always be true that \code{x \%matches\% specification(x)}.}

\value{object with class \code{spec keyed data.frame}}
\references{\url{http://metrumrg.googlecode.com}}

\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{as.spec}}
		\item \code{\link{specfile}}
		\item \code{\link{specification}}
		\item \code{\link{codes}}
		\item \code{\link{encode}}
		\item \code{\link{extract}}
		\item \code{\link{\%matches\%}}
		\item \code{\link{as.define.spec}}
	}
}
\examples{
example(`\%matches\%`)
}
\keyword{manip}

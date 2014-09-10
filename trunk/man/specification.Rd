\name{specification}
\alias{specification}
\alias{specification.default}
\alias{specification.comment}
\alias{specification.data.frame}
\alias{respecify}
\alias{respecify.character}
\alias{respecify.spec}

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
	\method{specification}{data.frame}(x, tol = 10, sep = '/', digits = 20, ...)
	\method{respecify}{character}(
		x, 
		data=sub('spec$','csv',x), 
		file=x, 
		...
	)
	\method{respecify}{spec}(x, data, file=NULL)
}

\arguments{
  \item{x}{object of dispatch}
  \item{tol}{consider an integer to be factor-like if it has this or fewer unique values.}
  \item{sep}{use this separator to \code{\link{encode}} factor-like objects.}
  \item{digits}{significant digits to retain for numeric range estimates}
  \item{data}{data.frame corresponding to x}
  \item{file}{filename for output}
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
It should always be true that \code{x \%matches\% specification(x)}.

If the underlying data changes, numeric ranges can be updated using \code{respecify}.
For a filename (character), the default behavior is to update the file in storage
and return the new spec invisibly.  Set file to NULL to leave storage untouched 
and return the spec visibly.  For class \code{spec}, NULL is already default for 
\code{file}, and \code{data} must be specified (cannot be guessed). 

}

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

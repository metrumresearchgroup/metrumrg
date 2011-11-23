\name{as.best.default}
\alias{as.best.default}
\alias{as.best.data.frame}
\alias{as.best}
\title{Coerce data unambiguously to numeric or character}
\description{
	In some sense, numeric and character are the most general
	data types.  \code{as.best} converts to one of these, handling
	missing or near-missing values systematically.~~
}
\usage{
	\method{as.best}{default}(x, ...)
	\method{as.best}{data.frame}(x, ...)
}

\arguments{
  \item{x}{object to be coerced}
  \item{\dots}{ignored}~~
}
\details{
Data printed at the command prompt can contain many ambiguities.  Factors
look just like character vectors.  Empty strings look just like spaces. Leading
or trailing white space can be confused with alignment adjustments. The string value
'NA' can be confused with NA. 

\code{as.best.default} coerces its argument to character, strips ~leading and 
trailing spaces, and coerces 'NA','',and '.' to NA. A numeric vector is returned 
if all non-missing values can be coerced to numeric.  A character vector is returned
if no non-missing values can be coerced to numeric.  An error is returned for 
mixed character-numeric, indicating the first character value.

The method for \code{data.frame} coerces each column in turn, returning either
the updated data frame or an error indicating the first problematic column.
}
\value{an object like x}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.character}}
		\item \code{\link{as.numeric}}
	}
}
\examples{
as.best(head(Theoph))
}
\keyword{manip}


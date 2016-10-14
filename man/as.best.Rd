\name{as.best.default}
\alias{as.best.default}
\alias{as.best.data.frame}
\alias{as.best}
%\alias{as.best.comment}
%\alias{as.best.timepoint}
\title{Coerce data unambiguously to numeric or character}
\description{
	In some sense, numeric and character are the most general
	data types.  \code{as.best} converts to one of these, handling
	missing or near-missing values systematically.
}
\usage{
	\method{as.best}{default}(x,prefix='#',na.strings=c('.','NA',''),...)
%	\method{as.best}{comment}(x, ...)
%	\method{as.best}{timepoint}(x, ...)
	\method{as.best}{data.frame}(x,...)
	
}

\arguments{
  \item{x}{object to be coerced}
  \item{prefix}{scalar character value to affix to numeric-like values in a vector containing non-numeric values}
  \item{na.strings}{character vector of values to treat as NA}
  \item{\dots}{ignored}
}
\details{
Data printed at the command prompt can contain many ambiguities.  Factors
look just like character vectors.  Empty strings look just like spaces. Leading
or trailing white space can be confused with alignment adjustments. The string value
'NA' can be confused with NA. Character 'TRUE' looks like logical \code{TRUE}.

\code{as.best.default} coerces its argument to character, strips leading and 
trailing white space, and coerces \code{na.strings} to NA. A numeric vector is returned 
if all non-missing values can be coerced to numeric.  A character vector is returned
if any non-missing values cannot be coerced to numeric.  If a prefix is supplied (the default)
numbers in a mixed vector are prefixed thus; if \code{prefix} is length zero, mixed
character-numeric gives an error.

The method for \code{data.frame} coerces each column in turn, returning either
the updated data frame or an error indicating the first problematic column. 
%Methods are supplied for \code{timepoint} and \code{comment}.
}
\value{an object like x}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.character}}
		\item \code{\link{as.numeric}}
		\item \code{\link{as.mDate}}
		\item \code{\link{as.comment}}
	}
}
\examples{
as.best(head(Theoph))
}
\keyword{manip}


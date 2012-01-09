\name{static}
\alias{static}
\alias{static.keyed}
\alias{static.data.frame}
\title{
Return the static portion of an object. 
}
\description{
Tables can be repetitive for structural reasons. Repetition means, for instance,
that a column value is not changing (i.e., is static) with respect to rows.  
The \code{data.frame} method for \code{static} detects which columns (maybe all)
are static with respect to \code{on}; it returns a unique subset of rows for just 
those columns. The method for \code{keyed} returns by default the columns that are 
static on the key (i.e., static with respect to the key).  \code{static} is conceptually
related to \code{constant}, which returns a logical rather than a data subset.
}
\usage{
	\method{static}{data.frame}(x,on=character(0), ...)
	\method{static}{keyed}(x,on=key(x),...)
}

\arguments{
\item{x}{object of dispatch}
\item{on}{character vector: column names in x}
\item{\dots}{ignored}
}

\details{
	By default, the \code{data.frame} method just returns unique rows.
	If \code{on} names columns in \code{x}, other columns are retained only if
	their values are static within combinations of values in the implied key;
	unique rows are returned. By default, the \code{keyed} method makes the 
	key explicit; it will be a non-operation if keys are defined and non-duplicated
	(i.e., all non-key columns are unique and therefore constant within the key,
	so the whole data.frame is returned).
}
\value{
	some subset of \code{x}
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{constant}}
			\item \code{\link{as.keyed}}
	}
}
\examples{
test <- as.keyed(
	data.frame(
		SUBJ=c(1,1,1,1,1,2,2,2,2,2),
		TIME=c(1,2,3,4,5,1,2,3,4,5),
		MALE=c(1,1,1,1,1,0,0,0,0,0),
		WT=c(70,70,70,70,70,65,65,65,65,65)
	),
	c('SUBJ','TIME')
)
identical(test,static(test))
static(test)
static(test,on='SUBJ')
}
\keyword{manip}


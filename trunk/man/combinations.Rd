\name{combinations}
\alias{combinations}

\title{Display Combinations of Column Values}
\description{
Generic, with method for data.frame.  Display sorted unique rows.
}
\usage{
 \method{combinations}{data.frame}(
 	x,
 	key=names(x),
 	depth=length(key),
 	detect=character(0),
 	count=FALSE,
 	...
 )
}

\arguments{
  \item{x}{object inheriting from data.frame}
  \item{key}{columns to show first (default sort criteria)}
  \item{depth}{integer up to length of key:  actual sort criteria}
  \item{detect}{columns to preprocess: 
  non-NA values will be replaced with 
  options('defined') or just 'defined'}
  \item{count}{whether to tabulate the number of each unique row; logical,
  or length-one character giving a column name for tabulations (default: 'count')}
  \item{\dots}{ignored}
}
\details{
Columns identified as 'key' are moved to the front (by default, all).

Sorting cascades across the key columns, using the second column to break ties,
etc.  Permutations of column order help to make patterns obvious.

Sorting can be limited to a subset of key columns using 'depth'.

Unique rows are displayed; the number of each such is tabulated as 'count' if 
\code{count} is TRUE.  Supply an alternate column name in place of TRUE, if you wish.

For column names in \code{detect}, all defined values are replaced with options('defined'),
which you can manipulate ('defined' by default).  Useful when one wishes to examine
patterns of missingness, rather than actual value details.

For keyed objects, the \code{detect} affect can be achieved more simply by subsetting
with a leading asterisk on column names.  See examples.
}
\value{keyed data.frame}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.keyed}}
	}
}
\examples{
combinations(
	iris[,c('Species','Petal.Width','Sepal.Length')],
	detect='Sepal.Length',
	count=TRUE
)
# alternatively ...
iris <- as.keyed(iris)
combinations(
	iris[,c('Species','Petal.Width','*Sepal.Length')],
	count=TRUE
)

}

\keyword{manip}


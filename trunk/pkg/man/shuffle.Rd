\name{shuffle}
\alias{shuffle}
\title{Move the Columns of a Data Frame Relative to Each Other}
\description{
It can be tedious to reorder the columns in a data.frame.  This function lets
you move specific columns relative to some other named column.
}
\usage{
shuffle(x, who, after = NA)
}
\arguments{
  \item{x}{data.frame}
  \item{who}{a character vector of column names to move, or a logical vector of length names(x), or a vector of indices}
  \item{after}{the column after which to put \code{who}: may be character, integer, NA, or NULL}
}
\details{
	If \code{after} is \code{NA}, the named columns are moved to the front (before the first column).
	If \code{after} has length zero, or is a number less than zero or an integer greater than the number of remaining
	columns, or is NULL, \code{who} is moved to the end. If an integer less than or equal to the number
	of remaining columns, \code{who} is placed after the corresponding column.
	
}
\value{
data.frame: a variant of x
}
\note{
  As of 5.56, duplicate column names are resolved by \code{make.unique} without warning.
 }
\references{\url{http://metrumrg.googlecode.com}}
\author{
Tim Bergsma
}
\seealso{
	\itemize{
		\item \code{\link{match}}
	}
}
\examples{
head(Theoph)
head(shuffle(Theoph,'Time'))
names(Theoph) <- c('Subject','Wt', 'Dose', 'Time',  'Time')
head(shuffle(Theoph,'Time'))

}
\keyword{manip}

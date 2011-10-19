\name{stableMerge}
\alias{stableMerge}
\title{Execute a Stable Left Join}
\description{
  Merge Two Data Sets Without Altering Material from the Left Argument.
}
\usage{
stableMerge(x, y)
}
\arguments{
  \item{x}{the main data.frame}
  \item{y}{a second data.frame containing related information}
}
\details{
	\code{merge} is unpredictable, because it can add, drop, and
	reorder rows, as well as reorder columns and recreate row names. 
	\code{stableMerge} expects a primary data.frame and
	a secondary data.frame, and performs a \sQuote{stable} left join (\code{all.x=TRUE}, 
	\code{all.y=FALSE}). Absence of extra arguments is by design, for integrity.
	Keys are defined by common columns, and are repeatable in the primary, 
	but must be unique in the secondary data.frame. New columns in the 
	secondary data.frame are appended to the primary, supplying secondary information 
	by key match.  No rows are added, or dropped, row/column order is 
	unaffected, and row names are preserved.
}
\value{
	A dataframe with as many rows as \code{nrow(x)} but possibly more columns.
}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{merge}}
	}
}
\examples{
#a nonsense example
stableMerge(Theoph,BOD)
}
\keyword{manip}


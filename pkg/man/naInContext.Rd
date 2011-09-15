\name{naInContext}
\alias{naInContext}
\title{Display Missing Values in Context}
\description{
  Display rows of data with missing values, as well as other rows with the same
  key.
}
\usage{
naInContext(x, context, search)
}
\arguments{
  \item{x}{data.frame.}
  \item{context}{character vector of column names representing a key}
  \item{search}{character vector of column names representing search fields.}
}
\details{
  Often one wants to view not just records with missing values, but
  other
  related records as well, e.g. \dQuote{view all the records for any subject
  with
  missing doses}.  \code{naInContext} searches for \code{NA}s in columns of \code{x}
  named by \code{search}.  It then determines keys for these rows, using
  columns in \code{x} named by \code{context}.  Finally, it displays all rows
  with matching keys.
}

\value{data.frame}
\author{Tim Bergsma}
\examples{
test <- Theoph
test$conc[[5]] <- NA
naInContext(test, context='Subject', search='conc')
}
\keyword{manip}


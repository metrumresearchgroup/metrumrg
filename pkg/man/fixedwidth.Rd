\name{fixedwidth}
\alias{fixedwidth.data.frame}
\alias{fixedwidth}
\title{Apply Fixed-width Formatting to a Data Frame}
\description{
  Generic, with a method for data.frame that right-justifies headers and columns,
  padding with spaces.
}
\usage{
\method{fixedwidth}{data.frame}(x, ...)
}
\arguments{
  \item{x}{a data.frame}
  \item{\dots}{extra arguments, ignored}
}
\details{
  There does not seem to be a way to get \code{write.table} to produce fixed-width output.
  \code{fixedwidth.data.frame} gives essentially the same result as 
  \code{print.data.frame}.  Columns and headers are padded, suitable for output
  to a file using the defaults for \code{write.table} (\code{sep=' '}).
}
\value{
  data.frame; all columns are character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{write.table}}
	}
}
\examples{
	fixedwidth(head(Theoph))
}
\keyword{manip}


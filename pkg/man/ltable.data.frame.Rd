\name{ltable.data.frame}
\alias{ltable.data.frame}
\title{Convert a Data Frame to a Latex Table}
\description{Convert data.frame to latex table.}
\usage{
\method{ltable}{data.frame}(
	x, 
	caption = NULL, 
	cap = caption, 
	cap.top = TRUE, 
	label = NULL, 
	options = "!htpb", 
	environments = "center", 
	file = NULL, 
	...
)
}
\arguments{
  \item{x}{data.frame}
  \item{caption}{full version of the caption}
  \item{cap}{short version of the caption, for list of tables}
  \item{cap.top}{Should caption be placed at the top, instead of bottom?}
  \item{label}{optional label}
  \item{options}{options for latex table environment}
  \item{environments}{extra environments to nest between \sQuote{table} and \sQuote{tabular}.}
  \item{file}{optional file name}
  \item{\dots}{passed to \code{tabular}}
}
\details{Converts data.frame to tabular, then wraps it in
specified environments, then wraps result in a latex table
environment.  Result is returned visibly or printed to file
and returned invisibly.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{tabular.data.frame}}
	}
}
\examples{
ltable(head(Theoph))
}
\keyword{manip}


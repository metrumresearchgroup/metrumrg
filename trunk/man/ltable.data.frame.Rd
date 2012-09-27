\name{ltable.data.frame}
\alias{ltable.data.frame}
\alias{ltable}
\alias{as.ltable}
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
	source = NULL,
	file = NULL,
	source.label='source: ',
	file.label='file: ',
	basefile=FALSE,
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
  \item{source}{optional source attribution}
  \item{file}{optional file name}
  \item{source.label}{optional text to preceed source if specified}
  \item{file.label}{optional text to preceed file if specified}
  \item{basefile}{if TRUE, strip path from file for display purposes}
  \item{\dots}{passed to \code{tabular}}
}
\details{Converts data.frame to tabular, then wraps it in
specified environments, then wraps result in a latex table
environment.  Result is returned visibly, or if \code{file} is specified
it is printed to file and returned invisibly.

If \code{source} and \code{source.label} are defined, they will be printed in a tiny font immediately
under the table (bound to the tabular element).  If \code{file} and \code{file.label} are defined as well,
they will be printed (tiny) under source.  Set \code{source.label} to NULL to 
suppress embedding of \code{source}; set to empty string to suppress source label. Set \code{file.label} to NULL to 
suppress embedding of \code{file}; set to empty string to suppress file label. Note that
\code{file} controls file destination, whether or not represented in the result.

\code{ltable} is generic.  You can write methods for other classes. 
\code{as.ltable} is an alias for \code{ltable}.
}
\value{character}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{tabular.data.frame}}
	}
}
\examples{
ltable(head(Theoph))
}
\keyword{manip}


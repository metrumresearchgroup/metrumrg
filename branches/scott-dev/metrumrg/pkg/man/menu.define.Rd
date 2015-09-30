\name{menu.define}
\alias{menu.define}
\alias{menu.define.spec}
\title{Work with Data Definitions}
\description{
This function is intended to work with \code{as.define} to create a data definition document as defined
by the US FDA for pharmaceutical drug submissions. For an existing \code{define} object, \code{menu.define} 
creates a header menu for a Define.pdf. Menu contains internal links to the underlying data definition 
tables as well as external local links to the stored data files.
}
\usage{
\method{menu.define}{define}(title='',desc=c(),dname=c())
}
\arguments{
  \item{title}{Title for the header row of the menu table.}
  \item{desc}{List of descriptions for each item in \code{dname}.}
  \item{dname}{list of data table names. Intended to match and link to the \code{caption} argument in \code{tabular.define}.}
}
\value{
Creates a latex table with one row per item in \code{dname}.
}
\note{ 
The \code{dname} argument creates a latex hypertarget-anchor pairing with the \code{caption} argument from \code{tabular.define}
in the 'Dataset' column of the table provided both values are identical. The \code{dname} argument also provides a latex external
link (href) to a like named file with a '.xpt' extension in the 'Location' column of the table.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Scott Pivirotto}
\seealso{
	\code{\link{tabular.define}}
}
\examples{
z <- menu.define(title='Test Project',desc=c("Analysis Data file"),dname=c("file1"))
writeLines(z)
}
\keyword{manip}


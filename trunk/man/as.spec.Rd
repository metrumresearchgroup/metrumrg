\name{as.spec}
\alias{as.spec}
\alias{as.spec.character}
\alias{as.spec.data.frame}
\alias{as.vector.spec}
\alias{read.spec}
\alias{write.spec}
\alias{summary.spec}
\title{
Create and Manipulate spec Objects
}
\description{
	A \code{spec} is a keyed data.frame giving an abstract specification of
	a data table.  It has columns \code{column, label, type, guide, required,
	and derivation}.  These are descriptive or prescriptive for the specified table.
	The \code{character} method for \code{as.spec} expects a scalar file name.
	\code{as.vector} returns the documented column names.  \code{read.spec}
	and \code{write.spec} use unquoted, tab-delimited cells, with `', `.', and 
	`NA' representing missing values (na strings).  The \code{summary} method
	does not worry about sort order. Generally one should not sort a \code{spec},
	since record order is informative.
}
\usage{
	\method{as.spec}{character}(x, ...)
	\method{as.spec}{data.frame}(x, ...)
	\method{as.vector}{spec}(x, mode = "any", ...)
	\method{summary}{spec}(object, ...)
	read.spec(x, unquote = TRUE,  ...)
	write.spec(x, file, ...)
}

\arguments{
\item{x}{object of dispatch}
\item{object}{object of dispatch}
\item{mode}{passed to \code{as.vector}}
\item{unquote}{whether to strip leading or trailing quote characters}
\item{file}{character}
\item{\dots}{ignored or passed}
}
\details{
	\code{column} gives the name and order of columns. 
	
	\code{label} is a descriptive substitute for column.  
	
	\code{type} is one of \code{character,numeric, integer, or datetime}, 
	where {datetime} indicates something in the \code{timepoint} class.
	
	\code{guide} is interpretive material:  typically units for numeric 
	columns, or \code{\link{encoded}} levels for factor-like columns.  \code{datetime} 
	columns expect a corresponding format string, as for \code{\link{as.mDateTime}}.
	Numeric guides are formally composed of an optional descriptor called \code{\link{guidetext}}
	followed by an optional range specifier in the form [0,30] or (0,30], etc.
	
	\code{required} is an enforceable presence specifier, typically 0 or 1.  It can be
	any R expression that can be coerced to logical and evaluated in the environment
	defined by the specified data frame.
	
	\code{derivation} is (non-tab) free text intended as a high-level
	description of how the column depends on other columns or data sources.  
	
	
}
\value{object of class \code{spec keyed data.frame} for \code{read.spec} and \code{as.spec}}
\references{\url{http://metrumrg.googlecode.com}}

\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{specfile}}
		\item \code{\link{specification}}
		\item \code{\link{codes}}
		\item \code{\link{encode}}
		\item \code{\link{\%matches\%}}
	}
}
\examples{
example(`\%matches\%`)
}
\keyword{manip}

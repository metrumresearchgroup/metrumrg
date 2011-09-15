\name{tabular.data.frame}
\alias{tabular.data.frame}
\title{Convert a Data Frame to a Latex Tabular Environment}
\description{Convert a data.frame to a latex tabular environment}
\usage{
\method{tabular}{data.frame}(
	x, 
	rules = c(2, 1, 1), 
	walls = 0, 
	grid = FALSE, 
	rowgroups = rownames(x), 
	colgroups = names(x), 
	rowbreaks = if (grid) breaks(rowgroups, ...) else 0,
	colbreaks = if (grid) breaks(colgroups, ...) else 0, 
	charjust = "left", 
	numjust = "right", 
	justify = ifelse(sapply(x, is.numeric), numjust, charjust), 
	colwidth = NA, 
	paralign = "top", 
	na = "", 
	verbatim = ifelse(sapply(x, is.numeric), TRUE, FALSE), 
	escape = "#", 
	trim = TRUE, 
	...
)
}
\arguments{
  \item{x}{data.frame}
  \item{rules}{numeric; will be recycled to length 3.  indicates number of 
  horizontal lines above and below the header, and below the last row.}
  \item{walls}{numeric, recycled to length 2.  Number of vertical lines
  on left and right of table.}
  \item{grid}{logical, whether to have lines between rows and columns}
  \item{rowgroups}{a vector as long as names(x), non-repeats trigger vertical lines}
  \item{colgroups}{a vector as long as nrow(x), non-repeats trigger horizontal lines}
  \item{rowbreaks}{numeric: a manual way to specify numbers of lines between rows (ignores grid and rowgroups)}
  \item{colbreaks}{numeric: a manual way to specify numbers of lines between columns (ignores grid and colgroups)}
  \item{charjust}{default justification for character columns}
  \item{numjust}{default justification for numeric columns}
  \item{justify}{manual specification of column justifications: left, right, center, or decimal (vector as long as ncol(x))}
  \item{colwidth}{manual specification of column width. (vector of length nrow(x).) Overrides \code{justify} where not NA.}
  \item{paralign}{used with colwidth to align paragraphs: top, middle, or bottom.}
  \item{na}{string to replace NA elements}
  \item{verbatim}{whether to use verbatim environment for numeric fields.  Makes sense for decimal justification; interacts with \code{trim} and \code{justify}.}
  \item{escape}{symbol used by `verb' command as delimiter.  A warning is issued if it is found in non-NA text.}
  \item{trim}{passed to the format command: true by default, so that 
  alignment is the responsibility of just the tabular environment arguments}
  \item{\dots}{passed to \code{format}}
}
\details{Principal choices here are the number of lines above and below the header,
number of lines at the end of the table (rules), and whether to have
lines between rows and columns (grid).  If you do want the latter, you 
can modify their placement easily with rowgroups and colgroups: factor-like
objects that show implicitly which sets of columns or rows go together. Neighboring
groups will be separated with a line.  For multiple lines at a given 
position, explicit control is offered by rowbreaks and colbreaks.  These 
latter have lengths one less than their respective dimensions.

Rownames are ignored.  If informative, capture them as a column.
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{align.decimal}}
			\item \code{\link{breaks}}
			\item \code{\link{row2tabular}}
			\item \code{\link{tabularformat}}
			\item \code{\link{wrap}}
	}
}
\examples{
tabular.data.frame(head(Theoph))
}
\keyword{manip}


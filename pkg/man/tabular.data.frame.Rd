\name{tabular.data.frame}
\alias{tabular.data.frame}
\alias{as.document}
\alias{as.document.tabular}
\alias{as.pdf}
\alias{as.pdf.document}
\alias{as.pdf.tabular}
\alias{as.pdf.data.frame}
\alias{tabular}
\title{Convert a Data Frame to a Latex Tabular Environment, Latex Document, or PDF}
\description{ The functions \code{tabular}, \code{as.document}, and \code{as.pdf}
are generic.  Methods are given here that cascade in the order data.frame -> tabular
-> document -> pdf.  \code{as.document.tabular} is argued identically to \code{tabular},
except for a few extra arguments; the function tries to guess the exact length and
width for a page size identical to the printed area (you can modify with the arguments
\code{wider} and \code{longer}).  \code{as.pdf.document} needs a file stem and 
(optionally) whether to perform clean-up.  The other \code{as.pdf} methods are 
convenience wrappers.
}
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
\method{as.document}{tabular}(
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
	wider = 0,
	longer = 0,
	prolog = NULL,
	epilog = NULL,	
	...
)
\method{as.pdf}{document}(x,stem,clean=TRUE,...)
\method{as.pdf}{tabular}(x,...)
\method{as.pdf}{data.frame}(x,...)
}
\arguments{
  \item{x}{data.frame}
  \item{rules}{numeric; will be recycled to length 3.  indicates number of 
  horizontal lines above and below the header, and below the last row.}
  \item{walls}{numeric, recycled to length 2.  Number of vertical lines
  on left and right of table.}
  \item{grid}{logical, whether to have lines between rows and columns}
  \item{rowgroups}{a vector as long as nrow(x), non-repeats trigger horizontal lines}
  \item{colgroups}{a vector as long as names(x), non-repeats trigger vertical lines}
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
  \item{wider}{additional document width in mm}
  \item{longer}{additional document lenth in mm}
  \item{prolog}{latex markup to include before the tabular environment}
  \item{epilog}{latex markup to include after the tabular environment}
  \item{stem}{the stem of a file name (no extension)}
  \item{clean}{whether to delete system files after pdf creation}
  \item{\dots}{passed to called functions}
}
\details{
Principal choices in \code{tabular.data.frame} are the number of lines above and below the header,
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
\references{\url{http://metrumrg.r-forge.r-project.org}}
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
tabular(head(Theoph))
}
\keyword{manip}


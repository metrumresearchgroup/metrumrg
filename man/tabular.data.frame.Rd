\name{tabular.data.frame}
\alias{tabular.data.frame}
\alias{tabular.define}
\alias{tabular.matrix}
\alias{tabular.table}
\alias{as.document}
\alias{as.document.character}
\alias{as.document.data.frame}
\alias{as.document.define}
\alias{as.pdf}
\alias{as.pdf.define}
\alias{as.pdf.document}
\alias{as.pdf.character}
\alias{as.pdf.data.frame}
\alias{tabular}
\alias{as.tabular}
\alias{tex2pdf}
\alias{makePreamble}
\alias{viewtex}

\title{Convert a Data Frame to a Latex Tabular Environment, Latex Document, or PDF}

\description{ The functions \code{tabular}, \code{as.document}, and \code{as.pdf}
are generic.  Methods are given here that cascade in the order data.frame -> tabular (character)
-> document -> pdf.  \code{as.document.data.frame} is argued identically to \code{tabular},
except for a few extra arguments; the function tries to guess the exact length and
width for a page size identical to the printed area (you can modify with the arguments
\code{wider} and \code{longer}).  \code{as.pdf.document} needs a file stem and 
(optionally) whether to perform clean-up.  The other \code{as.pdf} methods are 
convenience wrappers. \code{as.tabular} is an alias for \code{tabular}. See also
\code{\link{ltable}}, which wraps tabular output in a table environment.

\code{tex2pdf} accepts the file names of tex fragments. It reads those fragments, 
wraps them like documents and makes pdf files.  \code{viewtex} goes one step further, 
and tries to open those pdfs for viewing.

\code{tabular.table} reclassifies its argument as matrix.  \code{tabular.matrix} converts
its argument to data.frame.  If the rownames are suitably named, they are adopted as a 
column in the first position.
}
\usage{
\method{tabular}{data.frame}(
	x, 
	rules = c(2, 1, 1), 
	walls = 0, 
	grid = FALSE, 
	rowgroups = factor(rownames(x)), 
	colgroups = factor(names(x)), 
	rowbreaks = if (grid) breaks(rowgroups, ...) else 0,
	colbreaks = if (grid) breaks(colgroups, ...) else 0, 
        rowgrouprule = 0,
        colgrouprule = 0,
	rowcolors=NULL,	
	rowgrouplabel=' ',	
	charjust = "left", 
	numjust = "right", 
	justify = ifelse(sapply(x, is.numeric), numjust, charjust), 
	colwidth = NA, 
	paralign = "top", 
	na = "", 
	verbatim = ifelse(sapply(x, is.numeric), TRUE, FALSE), 
	escape = "#", 
	trim = TRUE, 
	source=NULL,
	file=NULL,
	source.label='source: ',
	file.label='file: ',
	basefile=FALSE,
	tabularEnvironment='tabular',
	footnote.size = 'tiny',
	...
)
\method{tabular}{define}(
  	x,
	caption = '',
	grid = TRUE,
	rules = 1,
	colwidth = c('1in','1in','0.5in','1.5in','1.5in'),
	tabularEnvironment = 'longtable',
	walls = 1,
	tabnum = FALSE,
	pretable = if(is.null(caption)) '' else 
	paste( if(tabnum) '\\\\caption{' else 
	'\\\\caption*{',caption,'}\\\\\\\\'),
	prepos = 1,
	headerBold = TRUE,
	...
)
\method{tabular}{matrix}(x, ...)
\method{tabular}{table}(x, ...)
\method{as.document}{data.frame}(
	  x,
	  rules = c(2, 1, 1), 
	  walls = 0, 
	  grid = FALSE, 
	  rowgroups = factor(rownames(x)), 
	  colgroups = factor(names(x)), 
	  rowbreaks = if (grid) breaks(rowgroups, ...) else 0,
	  colbreaks = if (grid) breaks(colgroups, ...) else 0, 
	  rowcolors=NULL,
	  charjust = "left", 
	  numjust = "right", 
	  justify = ifelse(sapply(x, is.numeric), numjust, charjust), 
	  colwidth = NA, 
	  paralign = "top", 
	  na = "", 
	  verbatim = ifelse(sapply(x, is.numeric), TRUE, FALSE), 
	  escape = "#", 
	  trim = TRUE, 
	  wider=0,
	  longer=0,
	  ...
)
\method{as.document}{character}(
	x,
	preamble=makePreamble(...),
	thispagestyle=command('thispagestyle',args='empty'),
	pagestyle=command('pagestyle',args='empty'),
	prolog=NULL,
	epilog=NULL,
	...
)
\method{as.document}{define}(
  	x,
  	morePreamble = command('usepackage',args = 'longtable'),
  	geoLeft = '1in',
  	geoRight = '1in',
  	geoTop = '1in',
  	geoBottom = '1in',
  	pagestyle = command("pagestyle", args = "plain"),
  	...
)
\method{as.pdf}{document}(x,stem,dir='.',clean=TRUE,...)
\method{as.pdf}{character}(x,stem,...)
\method{as.pdf}{data.frame}(x,stem,...)
\method{as.pdf}{define}(x,stem,...)
makePreamble(
  landscape=FALSE,
  wide=if(landscape) 279.4 else 215.9,
  long=if(landscape) 215.9 else 279.4,
  geoLeft = '1mm',
  geoRight = '1mm',
  geoTop = '1mm',
  geoBottom = '1mm',
  documentclass = command('documentclass',args='article'),
  xcolorPackage = command('usepackage',options=list('usenames',
  'dvispnames','svgnames','table'),args='xcolor'),
  geometryPackage = command(
    'usepackage',
    options=list(
      left=geoLeft,
      top=geoTop,
      bottom=geoBottom,
      right=geoRight
    ),
    args='geometry'
  ),
  geometry = command(
    'geometry',
    args=list(
      glue(
        'papersize=',
        glue('{',wide,'mm',',',long,'mm}')
      )
    )
  ),	
  multirow = command('usepackage',args='multirow'),
  morePreamble = NULL,
	...
)
tex2pdf(
	x,
	stem=NULL,
	dir=NULL,
	clean=TRUE,
	onefile=FALSE,
	...
)
viewtex(x,delete=TRUE,latency=1,...)
}
\arguments{
  \item{x}{object to be converted, typically data.frame (paths of tex files for \code{tex2pdf} and \code{viewtex})}
  \item{rules}{numeric; will be recycled to length 3.  indicates number of 
  horizontal lines above and below the header, and below the last row.}
  \item{walls}{numeric, recycled to length 2.  Number of vertical lines
  on left and right of table.}
  \item{grid}{logical, whether to have lines between rows and columns}
  \item{rowgroups}{a vector as long as nrow(x), non-repeats trigger horizontal lines}
  \item{colgroups}{a vector as long as names(x), non-repeats trigger vertical lines}
  \item{rowbreaks}{numeric: a manual way to specify numbers of lines between rows (ignores grid and rowgroups)}
  \item{colbreaks}{numeric: a manual way to specify numbers of lines between columns (ignores grid and colgroups)}
  \item{rowgrouprule}{number of lines to set off row group column, if rowgroups supplied as character}
  \item{colgrouprule}{number of lines to set off col group header, if colgroups supplied as character}
  \item{rowcolors}{character vector of color names, recycled as necessary to color all rows (NULL: no color)}
  \item{rowgrouplabel}{character string (at least one character) to label rowgroup column}
  \item{charjust}{default justification for character columns}
  \item{numjust}{default justification for numeric columns}
  \item{justify}{manual specification of column justifications: left, right, center, or decimal (vector as long as ncol(x))}
  \item{colwidth}{manual specification of column width. (vector of length ncol(x).) Overrides \code{justify} where not NA.}
  \item{paralign}{used with colwidth to align paragraphs: top, middle, or bottom.}
  \item{na}{string to replace NA elements}
  \item{verbatim}{whether to use verbatim environment for numeric fields.  Makes sense for decimal justification; interacts with \code{trim} and \code{justify}.}
  \item{escape}{symbol used by `verb' command as delimiter.  A warning is issued if it is found in non-NA text.}
  \item{trim}{passed to the format command: true by default, so that 
  alignment is the responsibility of just the tabular environment arguments}
  \item{source}{optional source attribution}
  \item{file}{optional file name}
  \item{source.label}{optional text to preceed source if specified}
  \item{file.label}{optional text to preceed file if specified}
  \item{basefile}{if TRUE, strip path from file for display purposes}
  \item{tabularEnvironment}{default \code{tabular}; consider also \code{longtable}}
  \item{caption}{for \code{define}, included in \code{pretable}}
  \item{tabnum}{for \code{define}, whether to number this table}
  \item{pretable}{for \code{define}, material to place before the table}
  \item{prepos}{for \code{define}, where exactly to append pretable material}
  \item{headerBold}{for \code{define}, whether to display the table header in bold font}
  \item{wide}{document width in mm}
  \item{long}{document lenth in mm}
  \item{wider}{additional document width in mm}
  \item{longer}{additional document lenth in mm}
  \item{preamble}{latex markup to include before beginning the document}
  \item{landscape}{if TRUE, default orientation is `landscape' not `portrait'}
  \item{geoLeft}{geometry package: left margin}
  \item{geoRight}{geometry package: right margin}
  \item{geoTop}{geometry package: top margin}
  \item{geoBottom}{geometry package: bottom margin}
  \item{documentclass}{document class command}
  \item{xcolorPackage}{xcolor package command}
  \item{geometryPackage}{geometry package command}
  \item{geometry}{geometry specification}
  \item{multirow}{multirow specification}
  \item{morePreamble}{additional preamble before beginning the document}
  \item{thispagestyle}{thispagestyle command}
  \item{pagestyle}{pagestyle command}
  \item{prolog}{latex markup to include before x}
  \item{epilog}{latex markup to include after x}
  \item{stem}{the stem of a file name (no extension)}
  \item{dir}{output directory}
  \item{clean}{whether to delete system files after pdf creation}
  \item{onefile}{whether to combine tex snippets into a single file}
  \item{delete}{whether temporary pdf (_doc.pdf) should persist}
  \item{latency}{how many seconds to wait before deleting temporary pdf}
  \item{footnote.size}{font size for source and file attributions}
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

As of v5.56, if you specify rowgroups or colgroups as a character vector, the 
appropriate margin will be explicitly subclassified (extra column for rowgroups
and extra header for colgroups).  If you also supply non-default column attributes 
(e.g. colbreaks, na, verbatim, colwidth, justify), add an extra element for the rowgroup column 
to overide internal defaults.  Row color (rowcolors) seems not to be 
entirely compatible with multirow cells (explicit rowgroups). For rowgroups,
be sure to use package \code{multirow} in your latex preamble.
 
If \code{source} and \code{source.label} are defined, they will be printed in a \code{footnote.size} font immediately
under the table (bound to the tabular element).  If \code{file} and \code{file.label} are defined as well,
they will be printed (same size) under source.  Set \code{source.label} to NULL to 
suppress embedding of \code{source}; set to empty string to suppress source label. Set \code{file.label} to NULL to 
suppress embedding of \code{file}; set to empty string to suppress file label. Note that
\code{file} controls file destination, whether or not represented in the result.

Rownames are ignored.  If informative, capture them as a column (done implicitly
for \code{tabular.table} and \code{tabular.matrix}).

Converting a \code{tabular} object or any character vector to a document causes
inclusion of default values for the the arguments of \code{makePreamble} and 
\code{as.document.character}.  Any of these can be dropped by passing NULL.  Invocation
of \code{makePreamble} can be avoided entirely by passing a value for \code{preamble}.
Alternatively, specify \code{morePreamble} to extend the preamble, or to replace any elements
you may have dropped.

}
\value{character}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{ltable}}
			\item \code{\link{align.decimal}}
			\item \code{\link{breaks}}
			\item \code{\link{row2tabular}}
			\item \code{\link{tabularformat}}
			\item \code{\link{wrap}}
			\item \code{\link{as.define}}
	}
}
\examples{
tabular(head(Theoph))
as.tabular(head(Theoph))
as.document(head(Theoph))
tabular(table(1:3,4:6))
tabular(head(Theoph,source='foo/bar',footnote.size='huge'))
\dontrun{as.pdf(head(Theoph),stem='theoph')}
\dontrun{metrumrgURL('example/tabular/tabular.pdf')}
}
\keyword{manip}


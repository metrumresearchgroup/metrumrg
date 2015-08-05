\name{xlog}
\alias{xlog}
\alias{xmlValue.XMLAttributeValue}
\alias{xpath2frame}
\title{Extract Indexed Values from XML}
\description{For recovered XML values, these functions maintain context by returning data frame columns that show the parentage of each value.}
\usage{

xpath2frame(
	x, 
	doc, 
	simplify = TRUE, 
	sort = TRUE, 
	nodebase = 'node',
	namespaces = FALSE,
	...
)

xlog(
	run, 
	project = getwd(), 
	rundir = filename(project, run), 
	file = filename(rundir, run, ".xml"), 
	xpath = c(
	"/output/nonmem/problem/problem_title",
	"/output/nonmem/problem/estimation/estimation_method", 
	"/output/nonmem/problem/estimation/estimation_title", 
	"/output/nonmem/problem/estimation/termination_status", 
	"/output/nonmem/problem/estimation/final_objective_function", 
	"/output/nonmem/problem/estimation/theta/val", 
	"/output/nonmem/problem/estimation/omega/row/col", 
	"/output/nonmem/problem/estimation/sigma/row/col"
	), 
	includeFile = NULL, 
	simplify = TRUE, 
	sort = TRUE, 
	nodebase = 'node',
	namespaces = FALSE,
	...
)
}
\arguments{
  \item{x}{a character vector of xpath expressions; omit namespace specifications if \code{namespaces} is FALSE}
  \item{doc}{an xml file path, a parsed xml tree, or a character vector to be parsed}
  \item{simplify}{whether to simplify the output by dropping static columns and stacking flag columns}
  \item{sort}{whether to sort the result}
  \item{nodebase}{a suggested base name for the stacking column; will be incremented using \code{.n} until unique}
  \item{namespaces}{whether to retain namespaces in parsed documents}
  \item{run}{character vector: run names}
  \item{project}{NONMEM project directory}
  \item{rundir}{character vector of run directories, normally constructed from
  run names and project directory}
  \item{file}{character vector of xml filenames, normally constructed from run
  names and run directories}
  \item{xpath}{character vector of xpath expressions}
  \item{includeFile}{xlog drops the column with the file name
  if it can recover the run name; includeFile=TRUE retains file name}
  \item{\dots}{extra arguments, ignored or passed}
}
\details{
  \code{xmlValue.XMLAttributeValue} is defined to support searches for attributes.
  
	A \code{keyed} data frame is returned, optionally sorted.  The key is
	all columns except 'value'.  If \code{simplify} is FALSE, it has
	columns representing every element of every search path.  For
	\code{xlog}, it also has \code{file} and \code{run} (or 
	\code{file.1, run.1, etc.} if these columns were already present).
	
	If \code{simplify} is TRUE, columns with only one unique value are dropped.
	Also, a column named like \code{nodebase} (e.g. \code{node}, or \code{node.1}, etc.,
	if \code{node} is already present) is introduced. Columns with exactly
	one unique value other than NA are dropped after placing the column name
	in corresponding rows of \code{node}. Columns beginning with \code{nodebase}
	come first, and therefore have priority when sorting.
	
	By default, namespaces are stripped from parsed XML, and so do not need to be
	retained in xpath expressions.  Alternatively, specify \code{namespaces=TRUE}
	and include namespaces in xpath expressions.  See examples.
	
  
}
\value{ object with class c('keyed','data.frame')}
\note{As per advice in the XML package: use of absolute xpath expressions may result 
in faster searches: e.g. "/output/nonmem/problem/problem_title" not "//problem_title".}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{xpathSApply}}
		\item \code{\link{xpath}}
		\item \code{\link{as.keyed}}

	}
}
\examples{
xpath2frame(
  '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:monitor/nm:obj',
  system.file('example/project/nonmem/1005/1005.xml',package='metrumrg'),
  namespaces = TRUE
)
libxmlVersion()
\dontrun{
# libxml 2.8 (others?) may be problematic. You can switch to full path notation
# as above, and use 'namespaces=TRUE' (not the default).
x <- xpath2frame(
  c(
    '/output/nonmem/problem/estimation/monitor/obj/@iteration',
    '/output/nonmem/problem/estimation/monitor/obj'
  ),
  system.file('example/project/nonmem/1005/1005.xml',package='metrumrg')
)
x
cast(x,obj~node)


xpath2frame(
  c(
    '/output/nonmem/problem/estimation/theta/val',
    '/output/nonmem/problem/estimation/omega/row/col',
    '/output/nonmem/problem/estimation/sigma/row/col'
  ),
  system.file('example/project/nonmem/1005/1005.xml',package='metrumrg')
)
project <- system.file('example/project/nonmem',package='metrumrg')
xlog(1005:1006,project = project)
}
}
\keyword{manip}


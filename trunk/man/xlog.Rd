\name{xlog}
\alias{xlog}
\alias{xmlValue.XMLAttributeValue}
\alias{xpath2frame}
\title{Extract Indexed Values from XML}
\description{For recovered XML values, these functions maintain context by returning data frame columns that show the parentage of each value.}
\usage{
xpath2frame(x, doc, simplify = TRUE, ...)

xlog(
	run, 
	project = getwd(), 
	rundir = filename(project, run), 
	file = filename(rundir, run, ".xml"), 
	xpath = c(
	"/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:estimation_method", 
	"/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:estimation_title", 
	"/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:termination_status", 
	"/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:final_objective_function", 
	"/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:theta/nm:val", 
	"/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:omega/nm:row/nm:col", 
	"/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:sigma/nm:row/nm:col"
	), 
	includeFile = NULL, 
	simplify = TRUE, 
	...
)
}
\arguments{
  \item{x}{a character vector of xpath expressions}
  \item{doc}{an xml file path, a parsed xml tree, or a character vector to be parsed}
  \item{simplify}{}
  \item{run}{character vector: run names}
  \item{project}{NONMEM project directory}
  \item{rundir}{character vector of run directories, normally constructed from
  run names and project directory}
  \item{file}{character vector of xml filenames, normally constructed from run
  names and run directories}
  \item{xpath}{character vector of xpath expressions}
  \item{includeFile}{xlog drops the column with the file name
  if it can recover the run name; includeFile=TRUE retains file name}
  \item{\dots}{extra arguments, ignored}
}
\details{
  

  \code{xmlValue.XMLAttributeValue} is~defined to support searches for attributes.
}
\value{
	A data frame is returned. If \code{simplify} is FALSE, it has
	columns representing every element of every search path.  For
	\code{xlog}, it also has \code{file} and \code{run} (prefixed by 
	\code{metrumrg:} if these columns were already present).
	
	If \code{simplify} is TRUE, columns with only one unique value are dropped.
	Also, a column \code{node} (or \code{metrumrg:node}
	if \code{node} is natively present) is introduced. Columns with exactly
	one unique value other than NA are dropped after placing the column name
	in corresponding rows of \code{node}. 
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{xpathSApply}}
		\item \code{\link{xpath}}

	}
}
\examples{
xpath2frame(
  c(
    '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:monitor/nm:obj/@nm:iteration',
    '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:monitor/nm:obj'
  ),
  system.file('example/project/nonmem/1005/1005.xml',package='metrumrg')
)
xpath2frame(
  c(
    '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:theta/nm:val',
    '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:omega/nm:row/nm:col',
    '/nm:output/nm:nonmem/nm:problem/nm:estimation/nm:sigma/nm:row/nm:col'
  ),
  system.file('example/project/nonmem/1005/1005.xml',package='metrumrg')
)
xpath <- '/nm:output/nm:nonmem/nm:problem/nm:problem_title'
project <- system.file('example/project/nonmem',package='metrumrg')
xlog(1005:1006,xpath=xpath, project=project)
xlog(1005:1006,project=project)


}
\keyword{ ~manip }


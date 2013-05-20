\name{xyplotExt}
\alias{xyplotExt}
\title{Plot the Parameter Search History for a NONMEM 7 Run}
\description{
NONMEM7 produces, for run \code{n}, the file \code{n.ext}, containing the values
of parameter estimates at specified iterations. Terminal estimates are included, and
possibly standard errrors.  This function plots the parameter estimates vs. iteration.
95 percent CI is plotted if standard errors are available.  Terminal estimate is 
indicated.
}
\usage{
xyplotExt(
	x, 
	data = read.table(file, skip = 1, header = TRUE, check.names = FALSE), 
	project = getwd(), 
	rundir = filename(project, x), 
	file = filename(rundir, x, '.ext'), 
	as.table = TRUE, 
	auto.key = TRUE, 
	layout = c(1, 4), 
	scales = list(relation = 'free'), 
	type = 'l', 
	panel = panel.superpose, 
	panel.groups = function(
		x, 
		y, 
		group.number, 
		type,
		...
	){
		if (group.number == 3) type <- 'p'
		panel.xyplot(x = x, y = y, type = type, ...)
	}, 
	...
)
}
\arguments{
  \item{x}{run name (number)}
  \item{data}{a data.frame representing an \code{*.ext} file}
  \item{project}{parent of run directory}
  \item{rundir}{run directory}
  \item{file}{path to \code{.ext} file}
  \item{as.table}{passed to \code{xyplot}}
  \item{auto.key}{passed to \code{xyplot}}
  \item{layout}{passed to \code{xyplot}}
  \item{scales}{passed to \code{xyplot}}
  \item{type}{passed to \code{xyplot}}
  \item{panel}{passed to \code{xyplot}}
  \item{panel.groups}{passed to \code{xyplot}}
  \item{\dots}{passed to \code{xyplot}}
}
\details{
If \code{data} is supplied, \code{x,project,rundir,file} are irrelevant. 
\code{data} is reshaped, and passed to \code{xyplot} with remaining arguments, 
all of which may be overridden.
}
\value{
a trellis object
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.pxml.ext}}
	}
}
\examples{
ext <- c(
'TABLE NO.  1: First Order: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION',
' ITERATION    THETA1       THETA2       THETA3       SIGMA(1,1)   OMEGA(1,1)   OBJ',
'            0  1.70E+0  1.02E-1  2.90E+1  0.00E+0  1.17E+0  11.570086639848398',
'            2  1.78E+0  1.06E-1  3.05E+1  0.00E+0  1.08E+0  9.377909428896904',
'            4  1.91E+0  1.05E-1  3.14E+1  0.00E+0  8.96E-1  8.983605357031118',
'            6  1.94E+0  1.01E-1  3.20E+1  0.00E+0  9.06E-1  8.940731060922468',
'            8  1.93E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940110966224346',
'           10  1.94E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940101673144566',
'           11  1.94E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940101673144566',
'  -1000000000  1.94E+0  1.01E-1  3.20E+1  0.00E+0  8.99E-1  8.940101673144566',
'  -1000000001  6.28E-1  7.36E-3  1.25E+0  0.00E+0  5.44E-1  0.'
)
file <- textConnection(ext)
data <- read.table(file, skip = 1, header = TRUE, check.names = FALSE)
close(file)
xyplotExt(data=data)
}
\keyword{manip}


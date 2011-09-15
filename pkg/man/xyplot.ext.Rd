\name{xyplot.ext}
\alias{xyplot.ext}
\title{Plot the Parameter Search History for a NONMEM 7 Run}
\description{
NONMEM7 produces, for run \code{n}, the file \code{n.ext}, containing the values
of parameter estimates at specified iterations. Terminal estimates are included, and
possibly standard errrors.  This function plots the parameter estimates vs. iteration.
95 percent CI is plotted if standard errors are available.  Terminal estimate is 
indicated.
}
\usage{
\method{xyplot}{ext}(
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
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\note{Even though \code{xyplot.ext} looks like a method, it will be more common
to call it directly, as in the examples.}
\seealso{
	\itemize{
		\item \code{\link{as.pxml.ext}}
	}
}
\examples{
ext <- c(
	'TABLE NO.  1: First Order: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION',
	' ITERATION    THETA1       THETA2       THETA3       SIGMA(1,1)   OMEGA(1,1)   OBJ',
	'            0  1.70000E+00  1.02000E-01  2.90000E+01  0.00000E+00  1.17001E+00  11.570086639848398',
	'            2  1.78158E+00  1.06239E-01  3.05314E+01  0.00000E+00  1.08862E+00  9.377909428896904',
	'            4  1.91182E+00  1.05179E-01  3.14289E+01  0.00000E+00  8.96680E-01  8.983605357031118',
	'            6  1.94836E+00  1.01426E-01  3.20728E+01  0.00000E+00  9.06374E-01  8.940731060922468',
	'            8  1.93983E+00  1.01742E-01  3.20128E+01  0.00000E+00  8.99988E-01  8.940110966224346',
	'           10  1.94057E+00  1.01681E-01  3.20217E+01  0.00000E+00  8.99322E-01  8.940101673144566',
	'           11  1.94057E+00  1.01681E-01  3.20217E+01  0.00000E+00  8.99322E-01  8.940101673144566',
	'  -1000000000  1.94057E+00  1.01681E-01  3.20217E+01  0.00000E+00  8.99322E-01  8.940101673144566',
	'  -1000000001  6.28499E-01  7.36368E-03  1.25313E+00  0.00000E+00  5.44628E-01  0.'
)
file <- textConnection(ext)
data <- read.table(file, skip = 1, header = TRUE, check.names = FALSE)
close(file)
xyplot.ext(data=data)
}
\keyword{manip}


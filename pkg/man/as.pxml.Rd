\name{as.pxml.ext}
\alias{as.pxml.ext}
\alias{as.pxml}
\title{Convert a Param File to XML}
\description{
  Convert the contents of a file to XML, encoding the leading lines as a \sQuote{note}
  element and the following lines as a table.
}
\usage{
as.pxml.ext(file, lead = 1, tag = 'param', ...)
}
\arguments{
  \item{file}{path to a parameter file}
  \item{lead}{number of leading \sQuote{note} lines}
  \item{tag}{name for the enclosing element}
  \item{\dots}{ignored}
}
\details{
  The \sQuote{note} and \sQuote{body} are identified, and coerced using \code{as.xml}.  The result
  is nested in an element with the name specified by \code{tag}.  The first column
  of \sQuote{body} is used as the key.
}
\value{
character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.xml}}
		\item \code{\link{xyplot.ext}}
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
pxml <- as.pxml.ext(file)
close(file)
pxml
}
\keyword{manip}


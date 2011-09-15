\name{plotfilename}
\alias{plotfilename}

\title{Make a Name for a Diagnostic Plot File}
\description{
 Make a name for a diagnostic plot file, given supplied arguments.
}
\usage{
plotfilename(
	run,
	dir=getwd(),
	grp=NULL,
	onefile=TRUE,
	stem='DiagnosticPlotReview',
	pext=if(onefile) '.pdf' else '_\%03d.pdf',
	...
)
}

\arguments{
  \item{run}{run name}
  \item{dir}{directory name}
  \item{grp}{filename modifiers, see NONR}
  \item{onefile}{includes a counter if \code{FALSE}}
  \item{stem}{filename stem}
  \item{pext}{extension, typically \sQuote{pdf}}
  \item{\dots}{ignored}
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
	}
}
\keyword{manip}


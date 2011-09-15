\name{dataFormat}
\alias{dataFormat}

\title{Integrate NONMEM Data for Plotting}
\description{
 Integrates data.frames representing standard NONMEM output and the source data.
}
\usage{
dataFormat(
	tabdata, 
	covdata, 
	pardata, 
	logtrans = FALSE, 
	grp = NULL, 
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99, 
	run, 
	...
)
}

\arguments{
	\item{tabdata}{data.frame for \file{*.TAB}}
  \item{covdata}{data.frame for underlying dataset,one record per ID}
  \item{pardata}{data.frame for \file{*par.TAB}}
  \item{logtrans}{see \code{PLOTR}}
  \item{grp}{see \code{PLOTR}}
  \item{grpnames}{see \code{PLOTR}}
  \item{cont.cov}{see \code{PLOTR}}
  \item{cat.cov}{see \code{PLOTR}}
  \item{par.list}{see \code{PLOTR}}
  \item{eta.list}{see \code{PLOTR}}
  \item{missing}{see \code{PLOTR}}
  \item{run}{see \code{PLOTR}}
  \item{\dots}{passed to \code{synthesis}}
}
\details{
	Called by \code{dataSynthesis}, and in turn calls \code{synthesis}.
}
\value{data.frame}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{dataSynthesis}}
		\item \code{\link{synthesis}}
	}
}
\keyword{manip}


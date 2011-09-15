\name{groupnames}
\alias{groupnames}

\title{Construct a Single Vector of Group Names from Possibly Multiple Indices to the Plotting Data Set}
\description{
 Callers of \code{PLOTR} may wish to group on more than one index.  This function combines
 those indices into text for a single index vector.
}
\usage{
groupnames(data, grp, grpnames = NULL, run)
}

\arguments{
  \item{data}{data.frame in which to find the groups}
  \item{grp}{columns by which to group}
  \item{grpnames}{optional substitute names}
  \item{run}{run name}
}
\value{factor}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
	}
}
\keyword{manip}


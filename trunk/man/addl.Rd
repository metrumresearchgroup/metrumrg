\name{addl}
\alias{addl}
\alias{addl.default}
\alias{addl.data.frame}
\title{
Calculate a NONMEM ADDL data item from explicit records. 
}
\description{
The NONMEM data item ADDL on a dose record expresses the number of additional
implicit doses that should follow at a regular interval II.  In the case
where explicit doses exist, \code{addl} supports compacting them into ADDL/II
notation.
}
\usage{
	\method{addl}{default}(x, interval=1,...)
	\method{addl}{data.frame}(
		x, 
		interval=24, 
		collapse=TRUE,
		cols=c('SUBJ','TIME','AMT','ADDL','II'),
		...
	)
}

\arguments{
\item{x}{numeric}
\item{interval}{numeric, e.g. hours}
\item{collapse}{boolean: drop explicit records that are implied by ADDL}
\item{cols}{conventional column names, as for \code{as.nm}}
\item{\dots}{ignored}
}

\details{
	For the default method, if an element of \code{x} is followed by n elements that occur 
	at a fixed interval \code{interval}, n is returned at that position
	and NA is returned for each of the following elements. n may be zero.
	
	It is the responsibility of the user to apply the function to meaningful
	subsets of the data, if any: e.g., sequential records with common subject and dose.
	
	For the data.frame method, an attempt is made to apply the default method,
	making assumptions about the structure of the data set. An error results
	if non-zero ADDL is calculated where ADDL is already non-zero. If \code{collapse} is TRUE,
	superfluous records are dropped; otherwise, their rownames are prefixed with "-".

}
\value{
	a numeric vector as long as \code{x}
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{reapply}}
	}
}
\examples{
test <- data.frame(
	SUBJ=c(1,1,1,1,1,2,2,2,2,2),
	TIME=c(1,2,3,4,5,1,2,3,4,5),
	EVID=c(1,1,1,1,0,1,1,1,1,0),
	AMT =c(1,1,1,1,0,1,1,1,2,0),
	DV  =c(0,0,0,0,1,0,0,0,0,1)
)
test
addl(test,interval=1)
addl(test,interval=1,collapse=FALSE)
}
\keyword{manip}


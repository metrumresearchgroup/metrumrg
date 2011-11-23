\name{addl}
\alias{addl}

\title{
Calculate a NONMEM ADDL data item from explicit records. ~~
}
\description{
The NONMEM data item ADDL on a dose record expresses the number of additional
implicit doses that should follow at a regular interval II.  In the case
where explicit doses exist, \code{addl} supports compacting them into ADDL/II
notation.
}
\usage{
addl(x, interval=1)
}

\arguments{
\item{x}{numeric}
  \item{interval}{numeric}
}

\details{
	If an element of \code{x} is followed by n elements that occur 
	at a fixed interval \code{interval}, n is returned at that position
	and NA is returned for each of the following elements. n may be zero.
	
	It is the responsibility of the user to apply the function to meaningful
	subsets of the data, if any: sequential records with common subject and dose.
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
test$II<-1
test$ADDL<-with(
	test,
	reapply(
		TIME,
		cumsum(runhead(paste(SUBJ,AMT))),
		addl,
		interval=1
	)
)
test
}
\keyword{manip}


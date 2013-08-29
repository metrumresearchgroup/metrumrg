\name{supp}
\alias{supp}
\alias{supp.data.frame}
\alias{supp.keyed}

\title{Merge CDISC Supplemental Data}
\description{
Generic, with method for data.frame and keyed.  

Within CDISC SDTM, data sets \code{SUPPxx} have a reasonably fixed relation to their 
counterparts \code{xx}, enabling an automated merge. The method for data.frame
defaults to the method for keyed, which returns the merge of the two data sets.
}
\usage{
 \method{supp}{data.frame}(
 	x,
 	suppx,
	...
 )
}
 \method{supp}{keyed}(
 	x,
 	suppx,
 	value= 'QVAL',  # possibly QORIG or QEVAL
	...
 )
}

\arguments{
  \item{x}{primary CDISC data set}
  \item{suppx}{supplemental CDISC data set}
  \item{value}{name of column in supplemental set where attribute names are stacked}
  \item{\dots}{ignored}
}

\value{keyed data.frame}
\references{
\url{http://metrumrg.googlecode.com}
\url{http://www.w3.org/wiki/images/6/61/HCLS$$F2F$$DrugSafety$SDTM_3.1_Implementation_Guide_v1_01.pdf}
See p. 124.
}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.keyed}}
	}
\examples{
\dontrun{
library(foreign)
ex <- read.xport('ex.xpt')
suppex <- read.xort('suppex.xpt')
ex <- supp(ex, suppex)
}	
}
\keyword{manip}


\name{iterations}
\alias{iterations}
\title{
Harvest Iteration Statistics from NONMEM Output and Convert from Covariance to Correlation
}
\description{
	\code{iterations} excises the relevant portion of the output file, converting
	the text to parameter and gradient estimates.
}
\usage{
iterations(x, ...)
}
\arguments{
  \item{x}{
character
}
  \item{\dots}{
ignored
}
}
\details{
The default result has intermediate dimensions as a compromise between 
very wide and very tall.
}
\value{
data frame indicating parameter estimates and gradients by iteration
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{cov2cor}}
	}
}
\examples{
lst <- c(
'0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.',
'1',
' ',
' ',
' #METH: First Order Conditional Estimation with Interaction',
' MONITORING OF SEARCH:',
'',
'0ITERATION NO.:    0    OBJECTIVE VALUE:   3140.31595361523        NO. OF FUNC. EVALS.:10',
' CUMULATIVE NO. OF FUNC. EVALS.:       10',
' PARAMETER:  1.0E-1  1.0E-1  1.0E-1  1.0E-1  1.0E-1  1.0E-1  1.0E-1  1.0E-1  1.0E-1  1.0E-1',
'             1.0E-1',
' GRADIENT:   3.9E+0 -4.0E+2  7.1E+2  1.8E+1 -1.6E+2  6.9E-1 -8.4E+0 -7.5E+1 -2.1E+2 -6.6E+2',
'            -3.9E+1',
'0ITERATION NO.:    5    OBJECTIVE VALUE:   2506.10546953541        NO. OF FUNC. EVALS.:11',
' CUMULATIVE NO. OF FUNC. EVALS.:       65',
' PARAMETER:  6.2E-2  1.1E+0 -1.0E+0 -1.1E+0  4.2E-1  1.2E-1  2.7E-1  7.1E-1  3.0E-1  1.3E-1',
'            -1.2E-1',
' GRADIENT:   8.6E+0  5.1E+1  2.9E+1 -3.5E+1  1.2E+1  2.2E+0 -2.5E+0  1.9E+1 -1.1E+1 -4.1E+0',
'            -1.3E+2',
'0ITERATION NO.:   10    OBJECTIVE VALUE:   2494.92785884531        NO. OF FUNC. EVALS.:12',
' CUMULATIVE NO. OF FUNC. EVALS.:      123',
' PARAMETER: -1.2E-1  9.9E-1 -1.1E+0 -1.2E+0  1.9E-1  1.6E-1  1.7E+0  5.0E-1  3.3E-1  1.2E-1',
'            -9.8E-2',
' GRADIENT:   1.2E+1  3.1E+1 -3.1E+1 -1.5E+1  1.9E-1  5.3E+0  3.2E+0  4.0E-1 -3.6E+0 -3.3E+0',
'            -8.1E+1',
'0ITERATION NO.:   15    OBJECTIVE VALUE:   2487.99745856896        NO. OF FUNC. EVALS.:11',
' CUMULATIVE NO. OF FUNC. EVALS.:      178',
' PARAMETER: -9.0E-2  9.8E-1 -1.0E+0 -1.0E+0  1.8E-1  1.0E-1  1.3E+0  4.9E-1  2.8E-1  1.8E-1',
'            -4.7E-2',
' GRADIENT:  -5.0E-1  2.2E-1 -1.7E+0  7.8E-1  6.4E-1 -1.3E-1 -1.1E-1  1.7E-1  1.7E-1  1.4E+0',
'             2.8E-1',
'0ITERATION NO.:   20    OBJECTIVE VALUE:   2487.98367110010        NO. OF FUNC. EVALS.:21',
' CUMULATIVE NO. OF FUNC. EVALS.:      246',
' PARAMETER: -8.8E-2  9.8E-1 -1.0E+0 -1.0E+0  1.7E-1  1.0E-1  1.3E+0  4.9E-1  2.8E-1  1.7E-1',
'            -4.7E-2',
' GRADIENT:  -5.8E-1 -1.1E+0 -4.6E+0 -4.7E-1 -2.2E-1 -1.2E-1 -2.9E-1  4.8E-2  1.5E-1 -5.1E-1',
'            -1.3E-1',
'0ITERATION NO.:   25    OBJECTIVE VALUE:   2487.88855559751        NO. OF FUNC. EVALS.:16',
' CUMULATIVE NO. OF FUNC. EVALS.:      340',
' PARAMETER: -8.8E-2  1.0E+0 -9.9E-1 -1.0E+0  1.7E-1  9.7E-2  1.4E+0  4.8E-1  2.7E-1  1.8E-1',
'            -4.6E-2',
' GRADIENT:  -9.4E-3  8.1E-3  4.5E-2 -1.9E-2 -7.8E-4 -1.0E-3 -7.2E-4  3.5E-3 -2.6E-3  1.1E-2',
'             9.0E-3',
' Elapsed estimation time in seconds:     3.11',
' ',
' #TERM:',
'0MINIMIZATION SUCCESSFUL',
' NO. OF FUNCTION EVALUATIONS USED:      340',
' NO. OF SIG. DIGITS IN FINAL EST.:  3.2',
'',
' ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,',
' AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.',
'',
' ETABAR:         2.3E-3  8.5E-4  6.8E-4',
' SE:             6.8E-2  4.6E-2  4.7E-2',
'',
' P VAL.:         9.7E-1  9.8E-1  9.8E-1',
' ',
' ETAshrink(\%):   7.3E-1  1.6E+1  6.5E+0',
' EPSshrink(\%):   8.8E+0',
' ',
' #TERE:',
' Elapsed covariance time in seconds:     4.27',
'1'
)
iterations <- iterations(lst)
iterations
it.dat <- melt(iterations,measure.var=names(iterations)[contains('X',names(iterations))])
xyplot(value~iteration|variable,it.dat[it.dat$course=='parameter',],
type='l',ylab='scaled parameter',as.table=TRUE,scales=list(y=list(relation='free')))
xyplot(value~iteration|variable,it.dat[it.dat$course=='gradient',] ,
type='l',ylab='gradient',as.table=TRUE,scales=list(y=list(relation='free')))

}
\keyword{manip}


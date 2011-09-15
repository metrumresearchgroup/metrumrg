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
\references{\url{http://mifuns.googlecode.com}}
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
' PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01',
'             1.0000E-01',
' GRADIENT:   3.9317E+00 -4.0782E+02  7.1408E+02  1.8310E+01 -1.6763E+02  6.9517E-01 -8.4428E+00 -7.5683E+01 -2.1314E+02 -6.6351E+02',
'            -3.9356E+01',
'0ITERATION NO.:    5    OBJECTIVE VALUE:   2506.10546953541        NO. OF FUNC. EVALS.:11',
' CUMULATIVE NO. OF FUNC. EVALS.:       65',
' PARAMETER:  6.2289E-02  1.1384E+00 -1.0060E+00 -1.1735E+00  4.2648E-01  1.2235E-01  2.7340E-01  7.1404E-01  3.0674E-01  1.3337E-01',
'            -1.2867E-01',
' GRADIENT:   8.6318E+00  5.1291E+01  2.9047E+01 -3.5090E+01  1.2660E+01  2.2857E+00 -2.5195E+00  1.9660E+01 -1.1005E+01 -4.1891E+00',
'            -1.3801E+02',
'0ITERATION NO.:   10    OBJECTIVE VALUE:   2494.92785884531        NO. OF FUNC. EVALS.:12',
' CUMULATIVE NO. OF FUNC. EVALS.:      123',
' PARAMETER: -1.2851E-01  9.9652E-01 -1.1333E+00 -1.2965E+00  1.9849E-01  1.6383E-01  1.7455E+00  5.0881E-01  3.3873E-01  1.2843E-01',
'            -9.8681E-02',
' GRADIENT:   1.2016E+01  3.1625E+01 -3.1473E+01 -1.5576E+01  1.9959E-01  5.3062E+00  3.2026E+00  4.0190E-01 -3.6134E+00 -3.3637E+00',
'            -8.1633E+01',
'0ITERATION NO.:   15    OBJECTIVE VALUE:   2487.99745856896        NO. OF FUNC. EVALS.:11',
' CUMULATIVE NO. OF FUNC. EVALS.:      178',
' PARAMETER: -9.0277E-02  9.8751E-01 -1.0251E+00 -1.0561E+00  1.8848E-01  1.0188E-01  1.3933E+00  4.9185E-01  2.8957E-01  1.8878E-01',
'            -4.7147E-02',
' GRADIENT:  -5.0246E-01  2.2589E-01 -1.7718E+00  7.8352E-01  6.4402E-01 -1.3915E-01 -1.1238E-01  1.7533E-01  1.7068E-01  1.4890E+00',
'             2.8887E-01',
'0ITERATION NO.:   20    OBJECTIVE VALUE:   2487.98367110010        NO. OF FUNC. EVALS.:21',
' CUMULATIVE NO. OF FUNC. EVALS.:      246',
' PARAMETER: -8.8682E-02  9.8198E-01 -1.0240E+00 -1.0659E+00  1.7898E-01  1.0123E-01  1.3935E+00  4.9043E-01  2.8974E-01  1.7010E-01',
'            -4.7448E-02',
' GRADIENT:  -5.8862E-01 -1.1992E+00 -4.6879E+00 -4.7829E-01 -2.2729E-01 -1.2550E-01 -2.9251E-01  4.8799E-02  1.5060E-01 -5.1096E-01',
'            -1.3573E-01',
'0ITERATION NO.:   25    OBJECTIVE VALUE:   2487.88855559751        NO. OF FUNC. EVALS.:16',
' CUMULATIVE NO. OF FUNC. EVALS.:      340',
' PARAMETER: -8.8040E-02  1.0105E+00 -9.9958E-01 -1.0162E+00  1.7939E-01  9.7973E-02  1.4278E+00  4.8858E-01  2.7835E-01  1.8430E-01',
'            -4.6731E-02',
' GRADIENT:  -9.4356E-03  8.1782E-03  4.5771E-02 -1.9044E-02 -7.8125E-04 -1.0624E-03 -7.2147E-04  3.5290E-03 -2.6681E-03  1.1598E-02',
'             9.0593E-03',
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
' ETABAR:         2.3774E-03  8.5597E-04  6.8554E-04',
' SE:             6.8571E-02  4.6635E-02  4.7602E-02',
'',
' P VAL.:         9.7234E-01  9.8536E-01  9.8851E-01',
' ',
' ETAshrink(\%):   7.3693E-01  1.6697E+01  6.5847E+00',
' EPSshrink(\%):   8.8722E+00',
' ',
' #TERE:',
' Elapsed covariance time in seconds:     4.27',
'1'
)
iterations <- iterations(lst)
iterations
it.dat <- melt(iterations,measure.var=names(iterations)[contains('X',names(iterations))])
xyplot(value~iteration|variable,it.dat[it.dat$course=='parameter',],type='l',ylab='scaled parameter',as.table=TRUE,scales=list(y=list(relation='free')))
xyplot(value~iteration|variable,it.dat[it.dat$course=='gradient',] ,type='l',ylab='gradient',as.table=TRUE,scales=list(y=list(relation='free')))

}
\keyword{manip}


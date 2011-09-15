\name{panel.densitystrip}
\alias{panel.densitystrip}
\alias{panel.covplot}
\alias{covplot}
\alias{panel.cuts}
\alias{panel.ref}
\alias{unitDensity}
\title{Display Distributions with Respect to Reference Values}
\description{
  These are panel functions that compare distributions with respect to 
  reference values.  \code{panel.densitystrip} plots a filled polygon for
  each unique value of \code{y}.  \code{panel.cuts} calculates the portion of
  each distribution (unique \code{y}) falling within specified limits.
  \code{panel.ref} shades a swath of the panel between specified limits of the 
  primary axis.  \code{covplot} uses the other panel functions to assemble 
  a sample display of covariate data.
}
\usage{
panel.densitystrip(
	x,
	y,
	horizontal,
	col.line,
	fill,
	factor,
	border=col.line,
	col=fill,
	...
)
panel.ref(
    x, 
    y, 
    col = 'grey90', 
    horizontal, 
    rlim, 
    ...
)
panel.cuts(
    x, 
    y, 
    cuts,
    col,
    col.line,
    text=col.line,
    horizontal = TRUE, 
    offset = -0.2, 
    increment = 0,
    format = function(x,...) as.numeric(round(x/sum(x) * 100)), 
    include.range = TRUE, 
    zero.rm = TRUE, 
    cex=0.7,
    ...
)
panel.covplot(
    x,
    y,
    ref=1,
    rlim=ref * c(0.75,1.25),
    cuts=ref * c(0.75,1,1.25),
    horizontal=TRUE,
    border='black',
    fill='grey',
    text='black',
    shade='grey90',
    col='white',
    ...
)
unitDensity(x,...)
}
\arguments{
  \item{x}{numeric}
  \item{y}{numeric}
  \item{horizontal}{if \code{TRUE}, strips run horizontally (x is primary axis)}
  \item{col.line}{
  	  \code{panel.density}: polygon border color if \code{border} not specified;
  	  \code{panel.cuts}: text color if \code{text} not specified
 }
  \item{fill}{polygon fill color if \code{col} not specified}
  \item{factor}{relative height of the density polygon}
  \item{border}{polygon border color}
  \item{col}{
  	\code{panel.density}: polygon fill color; 
	\code{panel.ref}: reference area fill color;
	\code{panel.covplot}: color of cut lines	
	\code{panel.cuts}: ignored	
 }
  \item{\dots}{extra arguments passed to other functions}
  \item{rlim}{length 2 vector: the limits of the shaded region}
  \item{cuts}{the values at which to divide the primary axis}
  \item{text}{text color for cut statistics}
  \item{offset}{distance from nominal value on secondary axis, at which to plot cut statistics}
  \item{increment}{distance from nominal value on primary axis, at which to plot cut statistics}
  \item{format}{a function to post-process counts of elements between cuts (should accept \dots)}
  \item{include.range}{if \code{TRUE}, \code{cuts} is supplemented with the range of the data}
  \item{zero.rm}{If \code{TRUE}, zeros are not printed} 
  \item{cex}{scale factor for text}
  \item{ref}{position of a black reference line; may be \code{NULL}}
  \item{shade}{reference fill color}
}
\details{
  Unlike \code{panel.densityplot}, \code{panel.densitystrip} has both \code{x}
  and \code{y} arguments.  In \code{panel.stripplot} and \code{panel.bwplot},
  panel data is implicitly subset by the discrete values on the secondary axis.
  I.e, visual elements are panel subsets.  Here, panel data is \emph{explicitly} subset
  using \code{panel.stratify}.  (The same pertains to 
  \code{panel.cuts}).  Densities are calculated using the default arguments
  of \code{density} (alternatives will be passed to \code{density} if
  supplied).  \code{unitDensity} rescales densities so that the maximum value is 
  one.  \code{panel.densitystrip} uses \code{factor} (traditionally used to control \code{jitter})
  to rescale densities again.
  
  A grouping variable can be used at the \sQuote{panel subset} level to give the same
  graphical parameters to several subsets, e.g., several polygons can share a color.
  
  
  For \code{panel.cuts}, calculated values are by default converted to character
  and printed below (\code{horizontal==TRUE}) the density strips.  Zeros are not printed
  by default, for less visual clutter; zeros are stripped before the conversion
  to character.  The actual values calculated are \sQuote{bin counts}, i.e., the number
  of elements in each vector that fall between adjacent cut points.  Only inner
  cuts need be specified, as the limits of the data are included by default as
  the outer limits.  Cuts are converted to percent by default; use 
  \code{format=function(x)x} to get actual counts. \code{cex} will control
  the size of the text.
  
  \code{panel.covplot} is optimized for a sample presentation of covariate effects.
  Values are assumed to be relative, so the center cut is 1 with +/- 25% reference
  region.  Five color features are specifiable.  \code{ref}, \code{rlim}, and \code{cuts}
  can be specified independently.  
  
  Formally, these panel functions are alternatives to \code{panel.stripplot}, and thus
  can be passed to \code{stripplot}. \code{xyplot} gives similar 
  results, and \code{bwplot} seems to give identical results.
  
  
}
\value{
  used for side-effects
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{densityplot}}
		\item \code{\link{bin}}
		\item \code{\link{panel.stratify}}
	}
}
\examples{
#a bootstrap of a pharmacokinetic model
set.seed(0)
boot <- data.frame(
	CL =      exp(rnorm(100,mean=3,    sd=0.25)),
	WT =   exp(rnorm(100,mean=-0.25,sd=0.25)),
	MALE = exp(rnorm(100,mean=0.3,  sd=0.25)),
	ASIAN =  exp(rnorm(100,mean=-.1,    sd=0.05))
)

#Model: CL = theta1 * (WT/70)**theta2 * theta3**MALE * theta4**ASIAN

#Normalize the structural parameter.
boot <- within(boot, CL <- CL/median(CL))

#Realize the submodel for non-normal instances of the continuous covariate.
boot <- within(boot, WT_35 <- (35/70) ** WT)
boot <- within(boot, WT_140 <- (140/70) ** WT)
boot$WT <- NULL

#(Categorical covariates are already expressed proportionally.)

#Reorganize the table.  rev() anticpates bottom-up plotting.
boot <- melt(rev(boot))

#Limit to 90% of the data, for independence from number of bootstraps.
boot <- boot[
	with(
		boot,
		value >= reapply(value,variable,quantile,0.05) &
		value <= reapply(value,variable,quantile,0.95)
	),
]

#plot using panel.covplot().
stripplot(
	variable~value,
	boot,
	panel=panel.covplot,
	xlab='relative clearance'
)
#with groups
stripplot(
	variable~value,
	boot,
	groups=contains('WT',variable),
	panel=panel.covplot,
	xlab='relative clearance'
)

#variations
data(crabs)
soup <- melt(crabs,id.var=c('sp','sex','index'))
soup$relative <- with(soup,value/mean(value[variable=='CL']))
soup$grp <- 'depth'
soup$grp[contains('W',soup$variable)] <- 'width'
soup$grp[contains('L',soup$variable)] <- 'length'
stripplot(variable~value,soup,panel=panel.stratify,panel.levels=panel.densitystrip)
stripplot(
	variable~relative|sp+sex,
	soup,
	panel=panel.stratify,
	panel.levels=panel.densitystrip
)
stripplot(
	variable~relative|sp+sex,
	soup,
	panel=panel.covplot
)
stripplot(
	variable~relative|sp+sex,
	soup,
	panel=panel.covplot,
	groups=grp,
	auto.key=TRUE,
	cex=0.5,
	offset=0.2
)
stripplot(
	value~variable|sp+sex,
	soup,
	groups=grp,
	panel=panel.covplot,
	auto.key=TRUE,
	horizontal=FALSE,
	ref=NULL,
	rlim=c(10,50),
	lty=3,
	lwd=2,
	border='transparent',
	text='blue',
	shade='turquoise',
	col='magenta',
	cuts=c(10,20,30,40,50)
)
}
\keyword{manip}


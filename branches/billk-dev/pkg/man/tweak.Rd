\name{tweak}
\alias{tweak}
\alias{tweak.initList}
\alias{tweak.init}
\alias{tweak.nmctl}

\title{Adjust Values}
\description{
\code{tweak} is generic.  Methods currently exist for classes \code{init}, 
\code{initList}, and \code{nmctl}. For the latter, to "tweak" a control stream
means to jitter the initial estimates.  By default, the estimate itself is scaled
by a value drawn at random from a normal distribution centered at 1 with standard 
deviation \code{sd}.  The default of 0.13 gives about +/- 10 percent change, on average.}
\usage{
\method{tweak}{init}(x,sd=0.13,digits=3,...)
\method{tweak}{initList}(x,sd=0.13,digits=3,...)
\method{tweak}{nmctl}(x,sd=0.13,digits=3,...)
}
\arguments{
  \item{x}{object of interest}
  \item{sd}{passed to \code{\link{rnorm}}}
  \item{digits}{passed to \code{\link{signif}}}
  \item{\dots}{passed to other functions}
}
\details{
If the tweaked value exceeds one of the bounds, another draw is taken by recursion.
To clear all bounds, set \code{fixed} to TRUE and then FALSE successively.
}
\value{
object of class \code{init}, or a numbered set of these, having class \code{initList}.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
  \itemize{
  	\item \code{\link{as.nmctl}}
	}
}
\examples{
path <- system.file('example/project/nonmem/ctl/1005.ctl',package='metrumrg')
ctl <- read.nmctl(path,parse=TRUE)
ctl$theta[] <- lapply(ctl$theta,`comment<-`,value=NULL)
set.seed(0)
tweak(ctl$theta)
x <- as.initList('(0,3) 2 FIXED (0 .6 1) 10 (-INF,-2.7,0) (37 FIXED) ')
x
fixed(x)
fixed(x) <- TRUE
fixed(x) <- FALSE
tweak(x)
}
\keyword{manip}

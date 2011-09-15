\name{diagnosticPlots}
\alias{diagnosticPlots}
\alias{covariatePlots}
\alias{cwresPlots}
\title{Create Diagnostic Plots}
\description{
  Create diagnostic plots from a data set.
}
\usage{
diagnosticPlots(
	data, 
	dvname='DV', 
	group=NULL, 
	model=NULL, 
	...
)

covariatePlots(
	data,
	cont.cov=NULL, 
	cat.cov=NULL,
	par.list=NULL, 
	eta.list=NULL, 
	...
)

cwresPlots(
	data,
	cont.cov=NULL, 
	cat.cov=NULL,
	...
)
}
\arguments{
	\item{data}{a data.frame, typically created by \code{dataSynthesis}}
  \item{dvname}{name of the dependent variable to use as a label for the diagnostic plots}
  \item{group}{optionally, a column name in \code{data} used to condition the output of \code{diagnosticPlots}}
  \item{model}{If supplied, this text argument will be prepended to the figure titles.}
  \item{cont.cov}{as defined for \code{PLOTR}}
  \item{cat.cov}{as defined for \code{PLOTR}}
  \item{par.list}{as defined for \code{PLOTR}}
  \item{eta.list}{as defined for \code{PLOTR}}
  \item{\dots}{ignored arguments}
}
\details{
	\code{PLOTR} passes the result of \code{dataSynthesis} to these functions and prints the
	resulting lists (of trellis objects) on the open device. 
}
\value{
a list of trellis objects
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
		\item \code{\link{dataSynthesis}}
	}
}
\keyword{manip}


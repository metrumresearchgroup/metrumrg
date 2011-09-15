\name{simpar}
\alias{simpar}
\alias{CreateParametersForSimulation}
\alias{TruncateParametersForSimulation}
\title{Create Parameters for Simulation with Uncertainty}
\description{
Given the parameter estimates of a mixed effect model, this function generates
sets of parameters for simulation.  Each set takes into account the relevant
variance-covariance structure of the fixed effects and random effects.  Two 
levels of random effects are supported, following the naming conventions of 
NONMEM.
}
\usage{
simpar(
	nsim, 
	theta, 
	covar, 
	omega, 
	sigma, 
	odf = NULL, 
	sdf = NULL, 
	digits = 4, 
	min = -Inf, 
	max = Inf
)
}
\arguments{
  \item{nsim}{scalar numeric specifying the number of sets to attempt}
  \item{theta}{vector of point estimates of fixed effect parameters}
  \item{covar}{variance-covariance matrix for fixed effect parameters}
  \item{omega}{list of variance-covariance matrices for first level random effects}
  \item{sigma}{list of variance-covariance matrices for second level random effects}
  \item{odf}{vector of omega degrees of freedom, one per matrix}
  \item{sdf}{vector of sigma degrees of freedom, one per matrix}
  \item{digits}{number of significant digits to include in output}
  \item{min}{lower limit for parameter estimates}
  \item{max}{upper limit for parameter estimates}
}
\details{
If \code{min} or \code{max} are non-default (see below), you may want to set \code{nsim} 
marginally higher to allow for dropped sets.

\code{covar} is coerced to matrix using \code{as.matrix}.

If \code{omega} and \code{sigma} are not lists, they are coerced using \code{list}.  Then
each element is coerced using \code{as.matrix}.

By default, each element in  \code{odf} and \code{sdf} will be the length 
(number of elements) in the corresponding matrix.

\code{min} and \code{max} may be given as scalar values, in which case they 
apply to all parameters (as do the defaults).  Alternatively, the first \eqn{n} limits
may be specified as a vector, in which case the remaining (if any) will be the default.
If any simulated parameter does not fall between its limits, inclusive, the entire
parameter set (row) is dropped from the result, with warning.
}
\value{
matrix, with column names indicating parameters, and row names indicating set
number before filtering by \code{min} and \code{max}.
}
\seealso{
	\itemize{
		\item \code{\link{posmat}}
		\item \code{\link{simblock}}
		\item \code{\link{as.matrix.halfmatrix}}
	}
}
\examples{
set.seed(100)
simpar(
    nsim=10,
    theta=c(13,75,1),
    covar=matrix(c(10,7,2,7,30,1,2,1,0.5),ncol=3,nrow=3),
    omega=list(
	0.1,
	matrix(c(0.04,0.02,0.02,0.04),ncol=2,nrow=2)
    ),
    odf=c(50,20),
    sigma=list(0.04,1),
    sdf=c(99,99),
    min=rep(0,3),
    max=rep(90,3)
)
simpar(
    nsim=1,
    theta=c(13,75,1),
    covar=matrix(c(10,7,2,7,30,1,2,1,0.5),ncol=3,nrow=3),
    omega=list(
	0.1,
	matrix(c(0.04,0.02,0.02,0.04),ncol=2,nrow=2)
    ),
    odf=c(50,20),
    sigma=list(0.04,1),
    sdf=c(99,99),
    min=rep(0,3),
    max=rep(90,3)
)
simpar(
    nsim=1,
    theta=c(13,75,1),
    covar=matrix(c(10,7,2,7,30,1,2,1,0.5),ncol=3,nrow=3),
    omega=list(
	0.1,
	matrix(c(0.04,0.02,0.02,0.04),ncol=2,nrow=2)
    ),
    odf=c(50,20),
    sigma=list(0.04,1),
    sdf=c(99,99),
    min=Inf,
    max=-1
)
}
\keyword{manip}


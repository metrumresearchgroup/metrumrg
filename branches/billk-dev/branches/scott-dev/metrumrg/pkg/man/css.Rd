\name{css}
\alias{acr}
\alias{cavg}
\alias{cmax}
\alias{cmin}
\alias{css}
\alias{ke}
\alias{tmax}
\alias{auc}
\title{Calculate One-Compartment Model Properties}
\description{
These functions calculate steady-state properties from the parameters of a 
one-compartment pharmacokinetic model, assuming all doses occur in the post-
distribution phase (tau >> tmax).
}
\usage{
acr(cl, v, tau, ...)
cavg(cl, tau, dose, ...)
cmax(cl, v, ka, tau, dose, ...)
cmin(cl, v, ka, tau, dose, ...)
css(cl, v, ka, tau, dose, time, ...)
ke(cl, v, ...)
tmax(cl, v, ka, tau, ...)
auc(cl, dose, ...)
}
\arguments{
  \item{cl}{apparent clearance (L/h)}
  \item{v}{apparent volume (L)}
  \item{ka}{absorption constant (\eqn{h^{-1}}{h^-1})}
  \item{tau}{steady-state trough-to-trough interval (h)}
  \item{dose}{dose (arbitrary units)}
  \item{time}{arbitrary time point, <= tau}
  \item{\dots}{other arguments, ignored}
}
\details{
\code{acr} calculates accumulation ratio (unitless, called by \code{css}).
\code{ke} calculates the elimination constant (\eqn{h^{-1}}{h^-1}, called by \code{css}).
\code{css} calculates steady-state compartment concentration (dose units per L) 
at a given time.  \code{cavg} calculates average concentration over dosing interval.
\code{cmin} and \code{cmax} calculate minimum and maximum steady-state concentrations; they
call \code{css}, passing either \code{tau} itself or \code{tmax} for the time argument.
\code{tmax} calculates the time of the maximum concentration.
\code{auc} calculates area under the curve.

Note that for the post-distributive assumption to hold, \code{tau} must be very large
relative to the time needed for drug disposition.  To the extent that it does not
hold, these functions give biased results.
}
\value{
numeric
}
\references{Gibaldi M, Perrier D. Multiple dosing. Pharmacokinetics. 
	New York: Marcel Dekker, Inc., 1982: p. 113- 144.}
\author{Tim Bergsma}
\note{
Multiple properties can be calculated within the same function or loop, since 
all arguments can be passed even if not needed.  For example, the same five
arguments can be passed, even un-named, to \code{cmax} and \code{tmax} (\code{tmax} will ignore dose).  
But be sure to pass by name the same arguments to \code{cmax} and \code{cavg}: the latter will
ignore unused arguments but has the common arguments in a different order.
}
\examples{
cavg(cl=0.05,tau=300,dose=100)
cmax(cl=0.05,v=10,ka=0.015,tau=300,dose=100)
cmin(cl=0.05,v=10,ka=0.015,tau=300,dose=100)
tmax(cl=0.05,v=10,ka=0.015,tau=300)
}

\keyword{manip}


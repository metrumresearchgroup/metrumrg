\name{accept}
\alias{accept}
\title{Document Acceptance of an R Installation}
\description{
  Execution of \code{accept} is a procedural act pertininent to installation qualification.
  The function logs an act of acceptance of the current installation, noting the 
  conditions of acceptance.  Currently, the conditions include the login of the 
  acceptor, the time of acceptance (GMT) and any contingent packages that 
  survive \code{packageCheck}.  
}
\usage{
accept(
	contingencies = c(
		'akima',
		'boot',
		'coda',
		'chron',
		'foreign',
		'fork',
		'lattice',
		'locfit',
		'MASS',
		'nlme',
		'plyr',
		'R2WinBUGS',
		'reshape',
		'SASxport',
		'survival'
	),
	installMissing=TRUE
)
}
\arguments{
  \item{contingencies}{a character vector of packages that must survive \code{packageCheck}
   in order for acceptance to succeed.  Can be \code{NULL}.  Defaults are arbitrarily chosen
  to suit the needs of Metrum Research Group, LLC.}
  \item{installMissing}{scalar logical: should \code{install.packages} be attempted for 
  missing contingencies?}
}
\details{
  The file \file{accept.xml} in \code{.Library} is created if it does not exist, and appended
  if it does.  In addition to the acceptor's login and the time, names and 
  versions of any contingent packages are stored.
  
  If \code{installMissing} is \code{TRUE}, an attempt will be made to install packages not in
  the default library (\code{.Library}), before loading is attempted.
}
\value{
  an invisible named vector of version identifiers, where the names are package
  names.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{packageCheck}}
		\item \code{\link{acceptance}}
	}
}
\keyword{manip}


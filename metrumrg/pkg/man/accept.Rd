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
	installMissing = TRUE,
	...,
	lib.loc = .libPaths(),
	lib = lib.loc[[1]],
	type = 'source'
)
}
\arguments{
  \item{contingencies}{a character vector of packages that must survive \code{packageCheck}
   in order for acceptance to succeed.  Can be \code{NULL}.  Defaults are arbitrarily chosen
  to suit the needs of Metrum Research Group LLC.}
  
  \item{installMissing}{scalar logical: should \code{install.packages} be attempted for 
  missing contingencies?}
  
  \item{\dots}{extra arguments, ignored}
  
  \item{lib.loc}{character vector of library trees to be searched; passed to 
  \code{installed.packages} and \code{packageCheck}}
  
  \item{lib}{character string for library to which any needed packages will be installed; Passed to
  \code{installed.packages}}
  
  \item{type}{passed to \code{install.packages}}
}
\details{
  The file \file{accept.xml} in \code{.Library} is created if it does not exist, and appended
  if it does.  In addition to the acceptor's login and the time, names, 
  versions, and paths of any contingent packages are stored.
  
  If \code{installMissing} is \code{TRUE}, the function notes the packages not in 
  \code{lib.loc} and tries to install them to \code{lib} before trying to load them.

}
\value{
  an invisible named vector of version identifiers, where the names are package
  names.
}
\note{
	No accommodation is made for the case where a package is installed in more 
	than one of the locations indicated by \code{lib.loc}.  Result will be
	determined by the default behaviors of \code{installed.packages} and 
	\code{library}, both of which accept \code{lib.loc} and presumably use
	the first encountered instance of any package name.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{packageCheck}}
		\item \code{\link{acceptance}}
	}
}
\keyword{manip}


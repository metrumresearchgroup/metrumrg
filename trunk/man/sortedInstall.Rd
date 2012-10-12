\name{sortedInstall}
\alias{sortedInstall}
\title{Install Local Source Packages Sorted by Dependency}

\description{Install packages from a local directory in proper dependency order.
Only intended for packages of type 'source', i.e. "*.tar.gz".}
\usage{
sortedInstall(pkgDir = getwd(), libDir=NULL, addLib = TRUE, tmpName = "tmpdir")
}
\arguments{
  \item{pkgDir}{directory where packages (\code{.tar.gz} extension) are located}
  \item{libDir}{directory where packages are to be installed (if \code{NULL} defaults to \code{pkgDir})}
  \item{addLib}{logical: add \code{pkgDir} to \code{.libPaths}?}
  \item{tmpName}{temporary file created in \code{pkgDir}}
}
\details{
	This function is an interactive way to recreate a particular package 
	environment, with fine control over package versions.  The user archives
	the packages locally. The function resolves the interdependencies
	to determine installation order.
	
	Naturally, the \code{metrumrg} package and its dependencies must be 
	attached in order to use \code{sortedInstall}. If these packages conflict 
	with the contents of \code{pkgDir}, users will need detach them before 
	attaching the desired versions. An easier solution may be closing the 
	current R session and opening a new one.

	If \code{pkgDir} contains more than one version of a package, the user 
	will be prompted to choose one to install.

	The user must have write privileges to \code{pkgDir}. 

	The function will destroy \code{tmpName}, so if \code{tmpdir} exists 
	in \code{pkgDir} it should probably be renamed.
}
\value{invisible \code{NULL}}
\references{\url{http://metrumrg.googlecode.com}}
\author{Natalie Hsiang}
\seealso{	\itemize{
			\item \code{\link{install.packages}}
	}
}
\keyword{manip}

\name{variants}
\alias{variants}
\title{Locate Variants of a File in Distinctive Subdirectories}
\description{
  Give the paths to identically named files distinguished by enclosing subdirectories,
  themselves enclosed by a directory bearing the file name.
}
\usage{
variants(x, path)
}
\arguments{
  \item{x}{a vector of file names}
  \item{path}{a directory enclosing subdirectories bearing the name(s) in \code{x}}
}
\details{
  Consider a file tree where identically named variants of a file are stored in
  distinguishing subdirectories, e.g. directories whose names contain timestamps.
  The enclosing directory itself bears the same name as the file in question.
  Thus, one variant of the file may be located at path/file/var1/file and a second
  at path/file/var2/file. Given \code{file} (\code{x}) and \code{path}, \code{variants} 
  returns both of these.  If \code{x} is of length greater than one, all variants
  are concatenated into a single character vector.
}
\value{
 character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{latest}}
	}
}
\keyword{manip}


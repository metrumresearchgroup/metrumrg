\name{getCovs}
\alias{getCovs}

\title{Return Non-commented Non-duplicated Rows of a NONMEM Data Set}
\description{
 Given a file and directory, retrieve the NONMEM source data set and limit
 to one row per subject.
}
\usage{
getCovs(file, dir)
}

\arguments{
  \item{file}{relative path to a data file}
  \item{dir}{directory to which that path is relative}
}
\details{
  The data file in a control stream is typically expressed relative to the 
  control stream's directory.  This tool resolves that reference and returns 
  the file, modified to represent constant covariates (comments stripped, first
  row per ID).
}
\value{data.frame}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{PLOTR}}
	}
}
\keyword{manip}


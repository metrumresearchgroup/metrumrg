\name{Tmax}
\alias{Tmax}
\title{Calculate Tmax}
\description{
 Calculate the Time Associated with the Maximum Concentration for each Subject
}
\usage{
Tmax(data, id = 'ID', dv = 'DV', time = 'TIME')
}
\arguments{
  \item{data}{the name of the R data.frame containing the data to use for the Tmax calculation.}
  \item{time}{chronologically ordered time variable present in \code{data}}
  \item{id}{variable in \code{data} defining subject level data}
  \item{dv}{dependent variable used to calculate Tmax present in \code{data}}	
}
\details{
	The \code{Tmax} function performs the calculation based on the variables \code{id}, \code{time}, and \code{dv} present in the R data object.
}
\value{One Tmax is returned for each subject.}
\references{\url{http://mifuns.googlecode.com}}
\author{Leonid Gibianski}
\keyword{manip}

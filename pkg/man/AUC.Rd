\name{AUC}
\alias{AUC}
\title{Calculate AUC Using the Trapezoidal Method}
\description{
  Calculate the area under the curve (AUC) for each subject over the time interval for
  \code{dv} using the trapezoidal rule. 
 }
\usage{
AUC(data, time = 'TIME', id = 'ID', dv = 'DV')
}
\arguments{
  \item{data}{data.frame containing the data to use for the AUC calculation}
  \item{time}{chronologically ordered time variable present in \code{data}}
  \item{id}{variable in \code{data} defining subject level data}
  \item{dv}{dependent variable used to calculate AUC present in \code{data}}
}
\details{
	The \code{AUC} function performs the calculation based on the variables \code{id}, \code{time}, and \code{dv} present in the R data object.
}
\value{
  One area under the dv-time curve is returned for each subject.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Leonid Gibianski}
\keyword{manip}

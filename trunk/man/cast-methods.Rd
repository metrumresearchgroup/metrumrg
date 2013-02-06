\name{cast-methods}
\docType{methods}
\alias{cast-methods}
\alias{cast,ANY-method}
\alias{cast,keyed-method}
\title{ Methods for Function \code{cast} in Package \pkg{reshape} }
\description{
 Methods for function \code{cast} in package \pkg{reshape}.  \pkg{metrumrg} converts \code{reshape::cast} to the default method for an S4 generic; it also promotes its own S3 class \code{keyed} to an S4 class, and defines a \code{cast} method for it.
}
\section{Methods}{
\describe{

\item{\code{signature(data = "ANY")}}{
For most purposes, a call to \code{cast} will dispatch \code{reshape::cast}, thus preserving existing behavior.  See \code{\link{cast}}.
}

\item{\code{signature(data = "keyed")}}{
The method for \code{keyed} mostly declares the same arguments as for \code{\link{cast}}.  The default for \code{fill}, however, is changed to \code{NA}. Three new arguments are added (before \dots).     \code{check.names=FALSE} and  \code{stringsAsFactors=FALSE} are passed to \code{\link{data.frame}} while reformatting the object returned by \code{cast}.    The new argument \code{reclass=TRUE} is used to post-process the result:  if \code{TRUE}, an attempt will be made to restore output columns to the class of the \code{value} column before cast's call to \code{\link{as.vector}}.  This method returns a keyed data.frame whose key is identical to \code{id.vars}.  See also \code{\link{melt.keyed}} and examples there.

}
}}
\keyword{methods}
\keyword{ manip }

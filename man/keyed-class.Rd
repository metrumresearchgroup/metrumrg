\name{keyed-class}
\Rdversion{1.1}
\docType{class}
\alias{keyed-class}

\title{Class \code{"keyed"}}
\description{
This class is a data frame with an associated key.  The key is the names of the columns that identify objects in the table.  Other columns presumably indicate attibutes of those objects.  The validity of the key is not enforced unless necessary.  The class is implemented as S3, and is promoted to S4 to take advantage of S4 generic mechanisms.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}}
    \item{\code{names}:}{Object of class \code{"character"}}
    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} }
    \item{\code{.S3Class}:}{Object of class \code{"character"} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{data.frame}"}, directly.
Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
  \itemize{
		\item \code{\link{as.keyed}}
	}
}
\examples{
showClass("keyed")
}
\keyword{classes}

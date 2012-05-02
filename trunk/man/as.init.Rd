\name{as.init}
\alias{as.init}
\alias{as.character.init}
\alias{as.character.initList}
\alias{as.init.numeric}
\alias{as.initList.character}
\alias{as.initList.list}
\alias{as.initList.numeric}
\alias{as.initList}
\alias{fixed.init}
\alias{fixed.initList}
\alias{fixed}
\alias{fixed<-.init}
\alias{fixed<-.initList}
\alias{fixed<-}
\alias{format.init}
\alias{format.initList}
\alias{print.init}
\alias{print.initList}
\alias{$.init}
\alias{$<-.init}
\alias{[.initList}

\title{Work with Initial Estimates as Objects Instead of Character Vectors}
\description{
\code{as.nmctl} and \code{read.nmctl} normally return a list of character vectors as class \code{nmctl}.  Users are free to modify these using conventional techniques for text.  The \code{theta} record can be converted to \code{initList} manually, or by the 
functions above, using \code{parse=TRUE}.  \code{initList} is a list of \code{init}, each of which displays conventionally, but can be modified using regular R syntax for lists and vectors.  In particular, if x is class \code{nmctl}, x$theta[[3]] refers to THETA3, not just the third line of text.
}
\usage{
as.init.numeric(x = numeric(0), fixed = FALSE, comment = character(0), ...)
as.initList.character(x, ...)
as.initList.numeric(x, fixed = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object of interest}
  \item{fixed}{logical: whether the value should be fixed at the initial estimate.}
  \item{comment}{character: a comment}
  \item{\dots}{ignored}
}
\details{

}
\value{
object of class \code{init}, or a numbered set of these of class \code{initList}.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\seealso{
  \itemize{
  	\item \code{\link{as.nmctl}}
	}
}
\examples{

}
\keyword{manip}

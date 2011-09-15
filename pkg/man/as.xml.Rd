\name{as.xml}
\alias{as.xml}
\alias{as.xml.default}
\alias{as.xml.character}
\alias{as.xml.data.frame}
\title{Represent an Object as XML}
\description{
  \code{as.xml} is generic, with a default method and methods for character
  and data.frame.  The intent is to convert text-like objects to informal
  XML that can be navigated with XPath expressions, e.g. using package XML.
}
\usage{
\method{as.xml}{default}(x, tag, ...)
\method{as.xml}{character}(x, tag, ...)
\method{as.xml}{data.frame}(x,keyname='row',key=rownames(x),...)
}
\arguments{
  \item{x}{an object to convert}
  \item{tag}{an XML element name for open and close tags in which to enclose 
  members of \code{x}; can be a vector}
  \item{keyname}{an XML element name to hold each value of each column in \code{x}}
  \item{key}{a vector of unique identifiers to distinguish each value of each column
  in \code{x}}
  \item{\dots}{name/value pairs to construct attributes in the \sQuote{open} tags 
  (ignored for the data.frame method)}
}
\details{
  The default method simply coerces its argument to character.
  
  The character method pastes \code{tag} as an XML element name (in brackets)
  before and after \code{x}.  Dots are used to construct attributes.  Like
  \code{tag} itself, the attributes can be vectors; in both cases the usual 
  recycling rules apply, as for \code{paste}.
  
  The data.frame method calls the character method on each of its columns, passing
  \code{keyname} and \code{keys} as \code{tag} and \code{key}, respectively.  \code{keys} should generally
  be unique.  Note that the columns have no parent: supply one manually with \code{nest},
  if necessary.
}
\value{
character
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.pxml.ext}}
	}
}
\examples{
nest(as.xml(head(Theoph)),tag='frame')
}
\keyword{manip}


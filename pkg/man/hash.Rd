\name{hash}
\alias{hash}
\title{Supply Comment Characters with Output}
\description{
  Any expression that generates output can be wrapped in a call to \code{hash}.  The usual
  output will be returned, with a hash character at the start of each line.
}
\usage{
hash(x,char='#')
}
\arguments{
  \item{x}{an R expression}
  \item{char}{text to prepend to each line}
}
\details{
  This is useful for embedding the result of an expression in a script.
}
\value{
used for side effects
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\section{Warning}{
  Don't \dQuote{hash} an expression that directly or indirectly calls a function being
  debugged.  The debug output will be \sQuote{sunk} to a connection.  If you get in 
  this situation, recover by typing \code{sink(NULL)} until output is restored.
}
\seealso{
	\itemize{
		\item \code{\link{file}}
		\item \code{\link{sink}}
	}
}
\keyword{manip}

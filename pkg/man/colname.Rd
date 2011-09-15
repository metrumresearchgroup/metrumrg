\name{colname<-}
\alias{colname<-}
\alias{name<-}
\alias{name}
\alias{colname}
\title{
Change Selected Names~~
}
\description{
Change selected element names.~~
}
\usage{
colname(x) <- value
}
\arguments{
  \item{x}{an object with named elements}
  \item{value}{named character vector}
}
\details{
Changing column names is a common task.  One can change them all at once, e.g.

\code{colnames(x) <- c(...)}

\code{names(x) <- c(...)}

or one at a time, e.g.

\code{names(x)[names(x)==old] <- new}

\pkg{reshape} supplies \code{rename}, a function that changes a subset of 
identified names, returning the object itself.

\code{library(reshape)}

\code{x <- rename(x, c(label=value,...))}

In contrast, \code{name} and its data frame equivalent \code{colname} change a
subset of identified names using assignment syntax.

\code{name(x) <- c(label=value,...)}

}
\value{
used for side effects
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{names}}
		\item \code{\link{colnames}}
	}
}
\examples{
names(Theoph)
name(Theoph) <- c(Subject='SUBJECT')
names(Theoph)
colname(Theoph) <- c(SUBJECT='subject',conc='concentration')
names(Theoph)
names(letters) <- LETTERS
letters
name(letters) <- c(A='a',B='b',C='c')
letters
}
\keyword{manip}


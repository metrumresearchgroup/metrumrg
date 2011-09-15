\name{as.flag}
\alias{[.flag}
\alias{[[.flag}
\alias{as.character.flag}
\alias{as.data.frame.flag}
\alias{as.flag.default}
\alias{as.flag.flag}
\alias{as.flag}
\alias{c.flag}
\alias{f}
\alias{format.flag}
\alias{print.flag}
\alias{rep.flag}
\alias{xtfrm.flag}

\title{Create and Manipulate Flags}
\description{
A flag is is an integer that may be imputed as zero where missing.
These functions implement the class.  Other functions may do the
imputation as necessary.
}
\usage{
\method{[}{flag}(x, ..., drop = TRUE)
\method{[[}{flag}(x, ..., drop = TRUE)
f(x, ...)
\method{as.character}{flag}(x, ...)
\method{as.data.frame}{flag}(x, row.names = NULL, optional = FALSE, ...)
\method{as.flag}{default}(x, ...)
\method{as.flag}{flag}(x, ...)
\method{c}{flag}(..., recursive = FALSE)
\method{format}{flag}(x, ...)
\method{print}{flag}(x, ...)
\method{rep}{flag}(x, ...)
}
\arguments{
  \item{x}{the flag object, or something coercible to integer}
  \item{\dots}{extra arguments, ususally ignored}
  \item{drop}{coerce to lowest possible dimension}
  \item{row.names}{row names for the result}
  \item{optional}{Optional to use the object name as the column name?}
  \item{recursive}{unused, present for historical reasons}

}

\details{
Typically a flag takes on the values of zero and 1, and is used
to indicate the presence of a particular condition.  Other functions
are welcome to impute \code{NA}s as 0. Methods are defined for common classes.  
\code{f} is an alias to \code{as.flag}.
}
\value{flag}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\examples{
hide(Theoph,where=Theoph$conc < 1,why='BLQ')
}
\keyword{manip}


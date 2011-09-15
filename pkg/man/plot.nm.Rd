\name{plot.nm}
\alias{plot.nm}
\title{Plot An Object of Class nm}
\description{This specializes the generic \code{plot} for nm objects. 
Standard arguments are derived and passed, together with extra arguments, 
to a suite of diagnostic plots \code{nmPlots}.
}
\usage{
\method{plot}{nm}(
	x, 
	which = NULL, 
	dvname = 'DV', 
	ivname = 'AMT', 
	covariates = NULL, 
	categorical = NULL, 
	continuous = NULL, 
	by = NULL, 
	...
)
}
\arguments{
  \item{x}{nm}
  \item{which}{names or numbers for subsetting \code{nmPlots}}
  \item{dvname}{column name for the dependent variable}
  \item{ivname}{column name for the independent variable}
  \item{covariates}{a list of covariates, guessed by default}
  \item{categorical}{a list of categorical covariates, guessed by default, ignored where not in \code{covariates}}
  \item{continuous}{a list of continuous covariates, guessed by default, ignored where not in \code{covariates}}
  \item{by}{column name(s) for extra plotting hierarchy (\pkg{lattice} levels); currently unimplemented}
  \item{\dots}{extra arguments passed to \code{nmPlots}}
}
\details{
Time within subject is rescaled to start at zero. Covariates are guessed to be 
everything that is non-structural. By default, covariates with 7 or more unique 
values are continuous, and the rest are categorical. But you can reallocate, 
even assigning the same column as both categorical and continuous.
}
\value{a list of \code{trellis} objects}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{nmPlots}}
		\item \code{\link{as.nm}}
	}
}
\keyword{manip}


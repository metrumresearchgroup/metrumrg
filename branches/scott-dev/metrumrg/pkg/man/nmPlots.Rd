\name{nmPlots}
\alias{nmPlots}
\title{An Extensible List of Diagnostic Plots for nm Objects}
\description{Default diagnostics for plotting an \code{nm} object.
\describe{
\item{kinetics}{dose and concentration data}
\item{constantContinuous}{continuous variables that are constant within \code{SUBJ}}
\item{varyingContinuous}{continous variables that vary within \code{SUBJ}}
\item{constantCategorical}{categorical variables that are constant within \code{SUBJ}}
\item{varyingCategorical}{categorical variables that vary within \code{SUBJ}}
}
}
\details{
\code{plot.nm} preprocesses an \code{nm} dataset and passes it to a subset of \code{nmPlots}. 
Extra arguments can be passed to \code{plot.nm} to exert fine control on the end results.
}
\seealso{
	\itemize{
		\item \code{\link{plot.nm}}
	}
}



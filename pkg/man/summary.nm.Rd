\name{summary.nm}
\alias{summary.nm}
\alias{naKeys.nm}
\alias{dupKeys.nm}
\alias{badAmt}
\alias{badAmt.nm}
\alias{badDv.nm}
\alias{badDv}
\alias{badII.nm}
\alias{badII}
\alias{falseAmt.nm}
\alias{falseAmt}
\alias{falseDv.nm}
\alias{falseDv}
\alias{noPk.nm}
\alias{noPk}
\alias{predoseDv.nm}
\alias{predoseDv}
\alias{print.nm.summary}
\alias{zeroAmt.nm}
\alias{zeroAmt}
\alias{zeroDv.nm}
\alias{zeroDv}


\title{Analyze an NM Object}
\description{
These methods test data set features related to use with NONMEM software((c) Icon 
Development Solutions).  The tests are performed after removing comments.
}
\usage{
\method{naKeys}{nm}(x, ...)
\method{dupKeys}{nm}(x, ...)
\method{badAmt}{nm}(x, ...)
\method{badDv}{nm}(x, ...)
\method{badII}{nm}(x, ...)
\method{falseAmt}{nm}(x, ...)
\method{falseDv}{nm}(x, ...)
\method{noPk}{nm}(x, ...)
\method{predoseDv}{nm}(x, ...)
\method{zeroAmt}{nm}(x, ...)
\method{zeroDv}{nm}(x, ...)
\method{summary}{nm}(object, by = NULL, ...)
\method{print}{nm.summary}(x, ...)
}
\arguments{
  \item{x}{object of class nm}
  \item{object}{object of class nm}
  \item{by}{character: column names in \code{object} on which to table row counts}
  \item{\dots}{extra arguments, ignored}
}
\details{
\describe{
   \item{badAmt}{\code{AMT} is \code{NA} where \code{EVID} is 1}
   \item{falseAmt}{\code{AMT} defined where \code{EVID} is not 1}
   \item{zeroAmt}{\code{AMT} is zero where \code{EVID} is 1}
   \item{badDv}{\code{DV} is \code{NA} where \code{EVID} is 0}
   \item{falseDv}{\code{DV} is defined where \code{EVID} is not 0}
   \item{zeroDv}{\code{DV} is zero where \code{EVID} is zero}
   \item{predoseDv}{\code{DV} is defined before the first record within \code{SUBJ} where \code{EVID} is 1; NA if no \code{EVID} records.}
   \item{noPk}{rows where \code{EVID} is never 0 within SUBJ}
   \item{badII}{\code{II} is greater than 0 where \code{ADDL} is \code{NA} or 0 (unless SS > 0)}
   \item{summary}{all of the above, plus some other diagnostics (see value)}
   \item{print}{pretty-printing for return value of \code{summary}}
}
}
\value{
  logical of length \code{nrow(x)} unless otherwise stated.  

  \code{print} method used for side effects.

  \code{summary} method: class nm.summary.
  
  \item{rows}{\code{nrow(x)}}
  \item{records}{number of active (non-commented) rows}
  \item{comments}{number of commented rows}
  \item{subjects}{number of unique \code{ID}, active rows}
  \item{longestCase}{maximum time range among \code{ID}, active rows}
  \item{naKeys}{as defined elsewhere, active rows}
  \item{dupKeys}{as defined elsewhere, active rows}
  \item{badDv}{as defined, active rows}
  \item{falseDv}{as defined, active rows}
  \item{zeroDv}{as defined, active rows}
  \item{predoseDv}{as defined, active rows}
  \item{badAmt}{as defined, active rows}
  \item{falseAmt}{as defined, active rows}
  \item{zeroAmt}{as defined, active rows}
  \item{noPk}{as defined, active rows}
  \item{badII}{as defined, active rows}
  \item{table}{counts of active rows tabled by \code{by}, if supplied}
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{as.nm}}
	}
}
\keyword{manip}


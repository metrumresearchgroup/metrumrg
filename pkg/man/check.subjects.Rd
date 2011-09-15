\name{check.subjects}
\alias{check.subjects}
\title{Summarize Columns of Subject Data}
\description{
  This function checks for missing data, numeric data, ranges, etc.
  It is a tool developed to review data that has been read into R.
}
\usage{
check.subjects(x,subject)
}
\arguments{
  \item{x}{data.frame}
  \item{subject}{subject identifier in the data (column name)}
}
\value{summary of each column in \code{x} by \code{subject}}
\references{\url{http://mifuns.googlecode.com}}
\author{Leonid Gibianski, modified by Tim Bergsma}
\keyword{manip}

\name{synthesis}
\alias{synthesis}

\title{Sequentially Left-join an Arbitrary Number of Data Frames, Picking Up Novel Columns}
\description{
 Integrates specified columns from among a list of data.frames.
}
\usage{
synthesis(x, key = character(0), frames, ...)
}

\arguments{
  \item{x}{A character vector of column names to seek}
  \item{key}{column names on which to merge}
  \item{frames}{a list of data frames to search}
  \item{\dots}{ignored}
}
\details{
  For each data.frame, sought columns as well as key columns are preserved,
  and the result is left-joined to the previous result (if any) after removing
  duplicate rows.
}
\value{data.frame}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{dataSynthesis}}
	}
}
\keyword{manip}


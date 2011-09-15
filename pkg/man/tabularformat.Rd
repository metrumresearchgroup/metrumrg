\name{tabularformat}
\alias{tabularformat}
\title{Create the Format String for Latex Tabular Environment}
\description{Create the format string for latex tabular environment.}
\usage{
tabularformat(justify, breaks, walls)
}
\arguments{
  \item{justify}{character vector of latex alignment symbols}
  \item{breaks}{numeric vector: number of vertical dividers at 
  each column break}
  \item{walls}{number of vertical lines for left and right edge of table}
}
\details{Used to set column alignment and aesthetics.}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{tabular.data.frame}}
	}
}
\keyword{manip}


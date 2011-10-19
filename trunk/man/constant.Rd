\name{constant}
\alias{\%nests\%}
\alias{crosses}
\alias{\%nested.in\%}
\alias{constant}
\alias{constant.default}
\title{Test for Nested Factors}
\description{
  Various Tests for Nested Factors.
}
\usage{
a \%nests\% b
a \%nested.in\% b
crosses(a,b)
\method{constant}{default}(x,within,...)
}
\arguments{
  \item{a}{a list of equal length factors, or one factor}
  \item{b}{a list of factors, or one factor, same length as \code{a}}
  \item{x}{an object like \code{a}, or a dataframe}
  \item{within}{An object like \code{b}, or just column names in \code{x} for the data.frame version}
  \item{\dots}{extra arguments passed to other methods}
}
\details{
  \code{nests} tests whether the factor(s) on the right are nested within the
  factor(s) on the left: i.e. \code{b} implies \code{a}. \code{nested.in} tests 
  the opposite: i.e., for unique interaction-wise levels of \code{a}, is there 
  exactly one interaction-wise corresponding level in \code{b} (\code{a} implies 
  \code{b})?  \code{constant}, by default, tests whether \code{within} implies 
  \code{x}.
  
  \code{crosses} is the basis for all other functions.  For each level
  in \code{b},
  it checks for corresponding levels of \code{a}, returning \code{FALSE} for
  the first
  and \code{TRUE} for any others, in which cases \code{a} crosses \code{b}, or
  \code{b}
  is crossed on (not nested in) \code{a}.
}

\value{
	Logical vector for \code{crosses}; scalar logical for all others.
}
\author{Tim Bergsma}
\note{
	All these functions are NA-safe; i.e., they treat \code{NA} as a distinct level.	
}

\examples{
CO2[,c('Type','Treatment')] \%nests\% CO2$Plant #TRUE
CO2$Plant \%nested.in\% CO2$Type #TRUE
with(CO2,constant(list(Type,Treatment),within=Plant)) #TRUE
with(ChickWeight,constant(Diet,within=Chick)) #TRUE
with(ChickWeight,constant(weight,within=list(Chick,Time))) #TRUE
crosses(c(1,1,1,NA,NA,4),c(2,3,3,NA,5,5)) #FALSE FALSE FALSE FALSE FALSE TRUE
constant(Theoph$Wt,within=Theoph$Subject)#TRUE
constant(Theoph, within='Subject')
}
\keyword{manip}


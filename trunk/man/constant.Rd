\name{constant}
\alias{crosses}
\alias{\%crosses\%}
\alias{\%crossed.on\%}
\alias{\%nests\%}
\alias{\%nested.in\%}
\alias{constant}
\alias{constant.default}
\title{Test for Nested Factors}
\description{
  Various Tests for Nested Factors.
}
\usage{
crosses(a,b)
a \%nests\% b
a \%nested.in\% b
\method{constant}{default}(x,within,...)
}
\arguments{
  \item{a}{a list of equal length factors, or one factor}
  \item{b}{a list of factors, or one factor, same length as \code{a}}
  \item{x}{an object like \code{a}, or a data.frame}
  \item{within}{An object like \code{b}}
  \item{\dots}{extra arguments passed to other methods}
}
\details{
  We propose an idiom pertaining to two factors (or interactions) of equal length,
  in which either a crosses b, or b nests a. Equivalently, in the former case 
  we could say b is crossed on/by a; in the latter we could say a is nested in b.
  In the case that b nests a, it must be true that b is constant within a. For example,
  if treatment nests replicate, than for multiple observations of a single level 
  of replicate, all observations of treatment must be the same. The value of 
  replicate implies the value of treatment (though the reverse need not be true).

  Formally, a crosses b if any level of a is associated with more than one level of b.
  
  \code{crosses()} is the basis for all other functions, and is useful for determining
 positions at which crossing occurs (or nesting fails). For each level of a, it returns
 FALSE for the first unique value of b, and TRUE for any others.
}

\value{
	Logical vector for \code{crosses()}; scalar logical for all others.
}
\references{\url{http://metrumrg.googlecode.com}}
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
crosses(c(2,3,3,NA,5,5),c(1,1,1,NA,NA,4)) #FALSE FALSE FALSE FALSE FALSE TRUE
constant(Theoph$Wt,within=Theoph$Subject)#TRUE
trt <- c('a','a','a','b')
rep <- c(1,1,2,3)
trt %crosses% rep # true
rep %nests% trt # false
trt %nests% rep # true
rep %nested.in trt # true
rep %crosses% trt # false
trt %crossed.on% rep #false
constant(trt,within=rep) # true

}
\keyword{manip}


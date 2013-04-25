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
crosses(a, b, all = FALSE)
a \%nests\% b
a \%nested.in\% b
a \%crosses\% b
a \%crossed.on\% b
\method{constant}{default}(x,within,...)
}
\arguments{
  \item{a}{a factor, something coercible to a factor, or a list of such having equal lengths (e.g. data.frame)}
  \item{b}{like \code{a}, possibly a different number of factors, but same length(s)}
  \item{all}{whether to include first instances: see details}
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
  In this case, b is not constant within a.  If you are wondering why 
  \code{!constant(x,y)}, examine \code{crosses(y,x)}.
  
  \code{crosses()} is the basis for all other functions, and is useful for determining
 positions at which crossing occurs (or nesting fails). For each level of a, it returns
 FALSE for the first unique value of b, and TRUE for any others. If all is TRUE,
 however, it returns TRUE for the first unique value of b when there are others. 


For the following equivalency table, all assertions in a row are true if any of them is true.
\tabular{lllll}{
\bold{crosses}	\tab \bold{crossed.on} \tab \bold{nests} \tab \bold{nested.in} \tab \bold{constant} \cr
a \%crosses\% b	\tab b \%crossed.on\% a \tab !b \%nests\% a \tab !a \%nested.in\% b \tab !constant(b,within = a) \cr
b \%crosses\% a	\tab a \%crossed.on\% b \tab !a \%nests\% b \tab !b \%nested.in\% a \tab !constant(a,within = b) \cr
!a \%crosses\% b \tab !b \%crossed.on\% a \tab b \%nests\% a \tab a \%nested.in\% b \tab constant(b,within = a) \cr
!b \%crosses\% a \tab !a \%crossed.on\% b \tab a \%nests\% b \tab b \%nested.in\% a \tab constant(a,within = b)
}
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
crosses(c(1,1,1),c(1,2,3)) # FALSE TRUE TRUE
crosses(c(1,1,1),c(1,2,2)) # FALSE TRUE TRUE
crosses(c(1,1,1),c(2,2,1)) # FALSE FALSE TRUE
crosses(c(1,1,1),c(1,2,3), all=TRUE) # TRUE TRUE TRUE
crosses(c(1,2,3),c(1,1,1)) # FALSE FALSE FALSE
crosses(c(NA,2,3),c(NA,NA,NA)) # FALSE FALSE FALSE
constant(Theoph$Wt,within=Theoph$Subject)#TRUE
trt <- c('a','a','a','b')
rep <- c(1,1,2,3)
trt \%crosses\% rep # TRUE
rep \%nests\% trt # FALSE
trt \%nests\% rep # TRUE
rep \%nested.in\% trt # TRUE
rep \%crosses\% trt # FALSE
trt \%crossed.on\% rep #FALSE
constant(trt,within=rep) # TRUE

}
\keyword{manip}


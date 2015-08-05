\name{ibw}
\alias{bmi}
\alias{bsa}
\alias{ibw}
\alias{lbm}
\alias{crcl}
\title{Calculate Various Clinical Indicators}
\description{
  Calculate body mass index, body surface area, ideal body weight, 
  lean body mass, or creatinine clearance using common equations.
}
\usage{
bmi(wt, ht)
bsa(wt, ht)
lbm(wt, ht, male)
ibw(ht, male, floor = FALSE)
crcl(age, wt, male, scr)
}
\arguments{
  \item{wt}{weight (actually, mass) in kilograms}
  \item{ht}{height in centimeters}
  \item{age}{age in years}
  \item{male}{logical indicating sex: \code{TRUE} indicates male}
  \item{floor}{if \code{TRUE}, \dQuote{inches over 5 feet} is not allowed to be negative.}
  \item{scr}{serum creatinine in mg/dL}
}
\details{
	Units of return values:
	\describe{
		\item{bmi}{\eqn{kg/m^2}}
		\item{bsa}{\eqn{m^2}}
		\item{lbm}{kg}
		\item{ibw}{kg}
		\item{crcl}{\eqn{mL/min}}
	}
}
\value{
  Numeric.  
}
\references{
	\describe{
		\item{bmi}{[\url{http://www.halls.md/body-mass-index/bmirefs.htm}]}
		\item{bsa}{(BSA: Gehan & George variation) \url{http://www.halls.md/body-surface-area/refs.htm}}
		\item{lbm}{\url{http://www.halls.md/body-mass-index/leanbody.htm}}
		\item{ibw}{\url{http://www.halls.md/ideal-weight/devine.htm}}
		\item{crcl}{\url{http://en.wikipedia.org/wiki/Renal_function}}
	}
}
\author{Tim Bergsma}
\examples{
bmi(70,160)
bsa(70,160)
lbm(70,160,TRUE)
ibw(160,TRUE)
crcl(35,70,TRUE,1.0)
}
\keyword{manip}


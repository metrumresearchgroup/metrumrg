\name{justUnits}
\alias{justUnits}
\alias{noUnits}
\alias{nospace}
\alias{lhs}
\alias{rhs}
\alias{tos}
\title{Extract Substring of Wiki Math}
\description{Extract logical portions of text formatted with wikimath conventions.}
\usage{
justUnits(x, ...)
noUnits(x, ...)
lhs(x, ...)
rhs(x, ...)
nospace(x, ...)
tos(x, ...)
}
\arguments{
  \item{x}{character; typically wikimath}
  \item{\dots}{ignoreded, or passed to \code{sub} for \code{lhs} and \code{rhs}}
}
\details{\code{justUnits} extracts the contents of the first parenthetical inclusion. 
	\code{noUnits} drops the first parenthetical inclusion.
	\code{nospace} removes all spaces.
	\code{tos} extracts the first occurrence of x_num, where x is 'theta', 'omega', or 'sigma' (case insensitive)
	and num is an unsigned decimal (1, 2.1, etc.).
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{wikiparse}}
		\item \code{\link{wiki2plotmath}}
		\item \code{\link{wiki2latex}}
	}
}
\examples{
wiki <- "CL/F (L) ~ theta_1 * (WT/70)^theta_2"
justUnits(wiki)
noUnits(wiki)
lhs(wiki)
rhs(wiki)
nospace(noUnits(lhs(wiki)))
tos(wiki)	
}
\keyword{manip}

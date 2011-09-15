\name{wikiparse}
\alias{wikiparse}
\alias{wiki2latex}
\alias{wiki2plotmath}
\alias{wiki2parameter}
\alias{wiki2label}
\alias{wikimath}
\title{Parse Wikimath}
\description{Parse wikimath, optionally overriding default style arguments.}
\usage{
wikiparse(x, sim = "~", dot = "*", pregreek = "", sup = "^", openSup = "{", closeSup = "}", sub = "_", openSub = "{", closeSub = "}", wrap = "", ...)
wiki2latex(x,...)
wiki2plotmath(x,...)
wiki2label(x,...)
wiki2parameter(x,...)
}
\arguments{
  \item{x}{character}
  \item{sim}{replaces wikimath tilde}
  \item{dot}{replaces wikimath asterisk}
  \item{pregreek}{prefixes theta, omega, sigma, eta}
  \item{sup}{invokes superscript}
  \item{openSup}{begins superscript content}
  \item{closeSup}{ends superscript content}
  \item{sub}{invokes subscript}
  \item{openSub}{begins subscript content}
  \item{closeSub}{ends subscript content}
  \item{wrap}{wraps entire string}
  \item{\dots}{ignored}
}
\details{
	Wikimath is a text coding convention for simple model statements
	of the form \code{"V_c /F (L * h^-1 ) ~theta_1 *(WT/70)^theta_2"}.
	The functions \code{wiki2latex} and \code{wiki2plotmath} are wrappers
	for \code{wikiparse} that intend to give nearly identical visual 
	representations in latex and plotmath contexts, respectively.  The functions
	\code{wiki2label} and \code{wiki2parameter} are extraction functions that
	give, e.g., \dQuote{V_c/F} and \dQuote{THETA1}.
	
	The point of a wikimath statement is to express a mathematical
	model definition for a quantity of interest, usually in terms of a parameter.
	Generally, a representation of the quantity is to the left of a tilde, 
	optionally with units in parentheses.  To the right of the tilde is a 
	submodel including relevant parameters and constants.  Parameters are limited
	to theta, omega, sigma, and eta, which should be subscripted.  The subscript
	operator in wikimath is the underscore.  The superscript operator is the
	"hat". Arithmetic operators generally represent themselves. 
	
	Subscripts and superscripts may be nested to somewhat arbitrary depth.
	However, to avoid ambiguity, nested levels must be terminated explicitly 
	by means of a space character. Thus \dQuote{V_c /F} is different from \dQuote{V_c/F }:  
	in the latter, \dQuote{/F} is erroneously part of the subscript.
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{justUnits}}
  	        \item \code{\link{closers}}
  	        \item \code{\link{wikitab}}
  	        \item \code{\link{tos}}
	}
}
\examples{
wiki <- "V_c /F (L * h^-1 ) ~theta_1 *(WT/70)^theta_2"
wiki2latex(wiki)
wiki2plotmath(wiki)
wiki2label(wiki)
wiki2parameter(wiki)
}
\keyword{manip}

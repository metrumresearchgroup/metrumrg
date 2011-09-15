\name{lookup}
\alias{lookup}
\alias{ctl2xml}
\alias{clear}
\alias{lookup.one}
\title{Lookup Alternate Versions of Parameter Codes}
\description{
  Parameter names have differing canonical forms depending on whether
  they are being used in a NONMEM control stream, an R plot, a latex table,
  etc.  The functions described here form a generalized interface for interconverting 
  these forms, given some xml-encoded data in a file, typically a control
  stream.  
}
\usage{
lookup(    x, within, by = 'name', as = NULL, type = 'parameter', ...)
lookup.one(x, within, by = 'name', as = NULL, type = 'parameter', ...)
ctl2xml(   x, ...)
clear(     x, drop = NULL, ...)
}
\arguments{
  \item{x}{a vector of parameter names (character)}
  \item{within}{an XML \sQuote{document} containing parameter elements (character)}
  \item{by}{the parameter attribute by which to interpret \code{x}}
  \item{as}{the parameter attribute as which to re-present \code{x}. If null, the text 
  value of the element (if any) is returned.}
  \item{type}{the element type to seek in \sQuote{within}}
  \item{\dots}{passed to other functions}
  \item{drop}{for \code{clear}: a vector of patterns to be replaced sequentially with
  \code{''} (nothing)}
}
\details{
  \code{clear} helps to isolate XML fragments from a text file.  \code{ctl2xml} puts it
  to use, assuming the fragments occur after the control stream comment character:
  it drops material up to and including the comment character, and then drops 
  everything up to the first '<'.  It is expected that a <parameter> declaration
  follows.
  
  \code{lookup.one} is the engine that researches elements of x, one at a time.
  
  \code{lookup} is the typical user interface to this system.  Try this.
  
  \itemize{
      \item In your control stream, create parameter comments for each element,
      e.g. \code{TVCL = THETA(1) ; <parameter name='THETA1' lattice='TH1' latex='\theta_1'>typical value of clearance</parameter>}
      
      \item In your script, use \code{readLines} to acquire your control stream
      as a character vector.
      
      \item Use \code{ctl2xml} to convert that vector to XML.
      
      \item Pass the converted vector to \code{lookup} as the argument \code{within}.
  }
}

\value{
All these functions return character.  \code{lookup} returns a vector of names 
corresponding to x, but having some other form (representing some other attribute).
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\examples{
codes <- c(
	'; etc <parameter name="THETA1" lattice="TH1" latex="\\\\Theta_1">effect of weight on clearance</parameter>',
	'and some other stuff',
	';<parameter name="OMEGA1" lattice="OM1" latex="\\\\Omega_1">interindivdual variance on volume</parameter>',
	'with maybe a < sign',
	';<parameter name="SIGMA1" lattice="SG1" latex="\\\\Sigma_1">additive error</parameter>'
)
codes
doc <- ctl2xml(codes)
lookup(c('TH1','SG1','OM1',NA),within=doc,by='lattice')
lookup(c('THETA1','SIGMA1','OMEGA1'),within=doc,as='lattice')
}
\keyword{manip}

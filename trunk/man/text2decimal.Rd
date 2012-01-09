\name{text2decimal}
\alias{text2decimal}
\title{Convert Mixed Text to Decimal}
\description{Convert character to numeric value where possible.}
\usage{
text2decimal(x)
}
\arguments{
  \item{x}{character}
}
\details{
\code{x} is coerced to character. Leading/trailing text is dropped; and the 
result is coerced to numeric. Useful, e.g., where measurements are reported with units. 

Leading text consists of characters not among \code{0:9}, \code{+},\code{-}, or \code{.}. 
The numeric region, if any, is the first consecutive string of those characters,
and may also include \code{e} or \code{E}. Any remaining characters are considered trailing.
Elements are substituted with their numeric regions, and the result is passed to
\code{as.numeric}. 

Changes were made as of version 5.9 to support scientific notation.  Note that \code{'e3'}
 and \code{'3e'} both give \code{3}: the first has 'e' stripped before coercion, and
 the second is valid syntax (e.g., \code{as.numeric('3e')}).

}
\value{numeric}
\references{\url{http://metrumrg.googlecode.com}}
\author{Tim Bergsma}
\examples{
text2decimal('every 3.5 hours')
text2decimal('-17 units')
x <- c(
  'I have -0.3 apples.',
  'I have -3 apples.',
  'I have 3. apples.',
  'I have +3 apples.',
  '3',
  'I have 3',
  '3 apples.',
  'I have e3 apples.',
  'I have 3e apples.',
  'I have +.3 apples.',
  'I have .3 apples.',
  'I have 0.3 apples.',
  'I have 1e3 apples.',
  'I have -1e-3 apples.',
  'I have 1E3 apples.',
  'I have 30e10 apples.',
  'I have 30 + apples.',
  'I have 30+ apples.',
  'I have ++.3 apples.',
  'I have +e3 apples.',
  'I have .3.2 apples.',
  'I have no apples.'
)
suppressWarnings(data.frame(x=x,as.decimal=text2decimal(x))) # last 5 are NA
}
\keyword{manip}


\name{codes}
\alias{codes}
\alias{codes.default}
\alias{codes.spec}
\alias{decodes}
\alias{decodes.default}
\alias{decodes.spec}
\alias{labels.spec}
\alias{guidetext}
\alias{guidetext.spec}
\alias{encoded.default}
\alias{encoded}
\alias{encoded.spec}

\title{Recover Metadata from spec Objects and their Components}

\description{A \code{spec} object has metadata stored compactly in its `label' and
`guide' columns.  Methods here extract metadata, acting on the relevant column
directly or on the object as a whole.}
\usage{
	\method{encoded}{default}(x, ...)
	\method{codes}{default}  (x, simplify=TRUE, ...)
	\method{decodes}{default}(x, simplify=TRUE, ...)
	\method{encoded}{spec}   (x, column=x$column, ...)
	\method{codes}{spec}     (x, column=x$column, ...)
	\method{decodes}{spec}   (x, column=x$column, ...)
	\method{labels}{spec}    (object, column=object$column, ...)
	\method{guidetext}{spec} (x, column=x$column, ...)
}

\arguments{
\item{x}{character, or spec}
\item{object}{character, or spec}
\item{simplify}{reduce a list to a vector if possible}
\item{column}{which column(s) to address}
\item{\dots}{ignored}
}
\details{
	\code{encoded} always returns TRUE or FALSE, telling whether the corresponding
	element represents an encoding of levels and labels.  See \code{\link{encode}}.
	\code{codes} returns a list(or vector) giving the element-wise codes (levels) or NA;
	\code{decodes} returns the corresponding decodes (labels) or NA.  \code{labels}
	returns the descriptors for column names; don't confuse with the labels for factor-like
	levels (decodes). \code{guidetext} returns the portion of a numeric guide element
	that is not a range specifier (if any):  e.g. just 'kg' for 'kg [0,30)'.
	
}
\value{ list or vector}
\references{\url{http://metrumrg.googlecode.com}}

\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{as.spec}}
		\item \code{\link{specfile}}
		\item \code{\link{specification}}
		\item \code{\link{encode}}
		\item \code{\link{extract}}
		\item \code{\link{\%matches\%}}
	}
}
\examples{
a <- encode(
  x = list(
    c('M','F'),
    c(1:4)
  ),
  labels = list(
    c('male','female'),
    c('caucasian','asian','african',NA)
  )
)
b <- encode(c(1:2),c('pediatric','adult'))
a
b
c <- c('a',NA,'##b##')
encoded(a)
encoded(b)
encoded(c)
encoded(' //4// ')
codes(a)
codes(b)
codes(b,simplify=FALSE)
codes(c)
codes('..1..')
decodes(a)
decodes(b)
decodes(c)
example(matches)
}
\keyword{manip}

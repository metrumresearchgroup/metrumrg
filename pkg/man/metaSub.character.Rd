\name{metaSub.character} 
\alias{as.filename}
\alias{as.filename.character}
\alias{metaSub.filename}
\alias{metaSub.character}
\alias{metaSub}
\title{Create Replicate Text files with Systematic Substitutions}
\description{
	\code{metaSub} is generic.  A method is defined for character; 
	a convenience wrapper is provided for passing names of text files to be 
	read and then resampled.
	
	\code{metaSub} collapses a character vector to one line of text.  
	The vector is replicated as many times as there are elements in \code{names}, 
	with flexible substitution of text fragments.  If \code{out} is supplied, the 
	replicates are saved as text files identified with \code{names}
	and a \code{suffix}.
	
	\code{metaSub.filename} will process multiple filenames, if \code{x} is as long as \code{names}. 
}
\usage{
	\method{as.filename}{character}(x, ...)
	\method{metaSub}{filename}(x, names, pattern=NULL, replacement=NULL, ...)
	\method{metaSub}{character}(
		x, 
		names, 
		pattern = NULL,
		replacement = NULL, 
		out = NULL, 
		suffix = '.txt', 
		fixed = TRUE, 
		...
	)
}

\arguments{
  \item{x}{scalar character, or (second form) filename(s). Multi-element character
  will be collapsed to one element, with newline as the separator.}

  \item{names}{a list of (unique) names for resulting output elements.
  A vector of names will be coerced to character and to list.}

  \item{pattern}{a character vector of text fragments to replace, optionally
  encoded as regular expressions (\code{fixed==FALSE}, See \code{?gsub}, \code{?regex}).  Can also be a list. See details.}

  \item{replacement}{A character vector of substitutions for patterns. 
  The wildcard \sQuote{*} is available to represent the corresponding value
  of \code{names}.  Can also be a list with as many elements as \code{pattern}
  (see details).  Expressions are supported (see details).}

  \item{out}{(path and) directory in which to write the resulting 
  control streams}

  \item{suffix}{file extension for filenames, if \code{out} is supplied}

  \item{fixed}{passed to \code{gsub}: use \code{FALSE} if \code{pattern} contains 
  regular expressions. Scalar, or same length as \code{pattern}.}

  \item{\dots}{extra arguments, available to expressions or passed to \code{gsub}}
}
\details{
	Typical usages are
	\preformatted{
		metaSub(x, names, ...)
		metaSub(as.filename(x), names, ...)
	}
	Replacement is performed by \code{gsub}, so an element of \code{pattern} 
	will be replaced everywhere it occurs in a line.
	
	if \code{pattern} or \code{replacement} is a list, each element should 
	be of length one, or as long as \code{names}.  In the latter case, 
	substitutions can be specific on a per-name basis.  The wild card \sQuote{*} is
	still available.
	
	It is necessary that \code{pattern} and \code{replacement} be of the 
	same length, but it is not necessary that their corresponding elements
	have equal lengths.  Thus, one can specify name-specific replacements 
	for a single pattern, or a single replacement for name-specific patterns.
	
	An expression can be specified for \code{replacement} itself, or one of 
	its pattern-wise elements, or one of the name-wise elements of a pattern-wise
	element.  Expressions are evaluated in an environment containing \dQuote{name}
	(same meaning as \sQuote{*} above) and all other \dots arguments. This is useful
	if extra arguments have a dimension indexed, at least loosely, by \code{names}.
	The evaluated expression is treated as character, and wildcard substitution
	is attempted. Use \code{\*} for a literal asterisk: in R: \code{\\\\*}.
	
	NOTE:  be very careful not to have trailing commas in your lists!  An 
	error results that is very hard to track.  e.g. \code{c(a,b,c,)}.
}
\value{
	an invisible named character vector.
	If \code{out} is supplied, elements are written as files with corresponding
	names.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{gsub}}
		\item \code{\link{regex}}
	}
}
\examples{
data(ctl)
dir()
e <- metaSub(
	ctl,
	names=1:3,
	pattern=c(
		'PROBLEM 8',
		'data8.csv',
		'8.MSF'
	),
	replacement=c(
		'PROBLEM *',
		'*.csv',
		'*.MSF'
	),
	out='.',
	suffix='.ctl'
)
t <- metaSub(
	ctl,
	names=c('test1','test2'),
	pattern=c('PROBLEM 8','data8.csv','METH=0'),
	replacement=c('PROBLEM *','*.csv','METH=1'),
)

t <- metaSub(
	ctl,
	names=c('test1','test2'),
	pattern=c(
		'PROBLEM 8',
		'data8.csv',
		'METH=0'
	),
	replacement=list(
		'PROBLEM *',
		'*.csv',
		c('METH=1','METH=2')
	),
	out='.',
	suffix='.ctl'
)
#just a file copy...
metaSub(as.filename('1.ctl'),names='4',out='.',suffix='.ctl')

#using a (nonsense) replacement expression...
options <- data.frame(var=c(8,9),alt=c(10,11))
a <- metaSub(
	ctl,
	names=rownames(options),
	pattern='9999', 
	replacement=expression(
		options$var[rownames(options)==name]
	),
	options=options
)
cat(a[[2]])

#replacement expression in a 'mixed' list...
b <- metaSub(
	ctl,
	names=rownames(options),
	pattern=list(
		'PRINT=2',
		'9999'
	), 
	replacement=list(
		'PRINT=3',
		expression(options$var[rownames(options)==name])
	),
	options=options
)
cat(b[[2]])

#replacement expressions on a per-name basis
d <- metaSub(
	ctl,
	names=rownames(options),
	pattern='9999',
	replacement=list(	#so that replacement is as long as pattern
		list(			#with different options for each 'name'
			expression(options$var[rownames(options)==name]),
			expression(options$alt[rownames(options)==name])
		)
	),
	options=options
)
cat(d[[2]])
}
\keyword{manip}

\name{resample.data.frame} 
\alias{resample}
\alias{as.csv.filename}
\alias{as.csv.filename.character}
\alias{resample.csv.filename}
\alias{resample.filename}
\alias{resample.data.frame}
\title{Create Replicate Data Sets by Stratified Sampling}
\description{
	\code{resample} is generic.  A method is defined for data.frame; 
	a convienience wrapper is provided for passing names of files 
	to be read and then resampled.
}
\usage{
	\method{as.csv.filename}{character}(x, ...)
	\method{resample}{csv.filename}(x, ...)
	\method{resample}{data.frame}(
	x, 
	names, 
	key = NULL, 
	rekey =FALSE, 
	out = NULL, 
	stratify = NULL, 
	ext='.csv',
	row.names=FALSE, 
	quote=FALSE, 
	sep=',',
	replace=TRUE,
	...
)
}

\arguments{
  \item{x}{a data.frame, or (second/third form) a file name for a file to read}
  \item{names}{a list of names for replicate data sets; can be a simple vector}
  \item{key}{a scalar character value naming the column in x that distinguishes
  unique individuals, (resampling targets); defaults to row names}
  \item{rekey}{If true, key values in resampled data sets will have unique values 
  of key replaced with consecutive integers, starting at 1.}
  \item{out}{a (path and) directory in which to write resulting data sets}
  \item{stratify}{A list of factors, the interactions of which will be the levels of 
  	  stratification.  Each factor must have the same length as \code{nrow(x)}.  Or a character vector
  of names in \code{names(x)}.}
  \item{ext}{a file extension}
  \item{row.names}{passed to \code{write.table}}
  \item{quote}{passed to \code{write.table}}
  \item{sep}{passed to \code{write.table}}
  \item{replace}{passed to \code{sample}}
  \item{\dots}{extra arguments, passed to \code{sample} and \code{write.table}}
}
\details{
	Typical usages are
	\preformatted{
		resample(x, names, ...)
		resample(as.csv.filename(x), names, ...)
		resample(as.filename(x), names, ...)
	}
	The argument \code{key} gives the name of the column in \code{x} to identify
	unique experimental units (individuals). If not supplied, a temporary
	key is constructed from the row names, and sampling occurs at the row level.
	
	The number of resamplings is controlled by the length of \code{names}. 
	\code{names} is coerced to character, and each value is used to name 
	a \file{*.csv} file, if \code{out} is supplied. If \code{out} is omitted, a list
	of data.frames is returned.
	
	\code{stratify} is a list of factors, or items that can be coerced to 
	factors.  Currently \code{stratify} is coerced to a data.frame for
	convenient manipulation.  Empty levels are dropped. If \code{stratify} is 
	not supplied, the whole data set is treated as a single level. Otherwise,
	each resulting data set has as many keys in each level as the original.
	An error results if \code{key} is not nested within \code{stratify}.
	
	The default behavior is to sample with replacement (\code{replace=TRUE}.)
	This and other arguments to \code{sample} can be modified.
}
\value{
	A list of data.frames, or if \code{out} is supplied, an invisible list of
	the numbers of rows of each data.frame written to file.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{metaSub.character}}
		\item \code{\link{sample}}
	}
}
\examples{
b <- resample(Theoph,key='Subject',names=1:3)
d <- resample(
	Theoph,
	key='Subject',
	rekey=TRUE,
	names=1:3,
	out='.',
	stratify=Theoph$Dose < mean(Theoph$Dose)
)
e <- resample(as.csv.filename('1.csv'),names='theoph')
}
\keyword{manip}

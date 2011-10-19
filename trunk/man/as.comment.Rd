\name{as.comment}
\alias{[.comment}
\alias{[[.comment}
\alias{as.character.comment}
\alias{as.comment.comment}
\alias{as.comment.default}
\alias{as.comment}
\alias{as.data.frame.comment}
\alias{c.comment}
\alias{format.comment}
\alias{hide.data.frame}
\alias{hide}
\alias{print.comment}
\alias{rep.comment}
\alias{xtfrm.comment}
\alias{hidden}
\alias{hidden.data.frame}
\alias{summary.hidden.data.frame}

\title{Create and Manipulate Comment Objects}
\description{Some conventions include the use of \sQuote{C} or \sQuote{.} as the 
first element in a row. \sQuote{C} is typically used to identify a row to be 
ignored \dQuote{commented-out}. Here, a comment is really a logical that should 
never be \code{NA}, and prints as \sQuote{C} or \sQuote{.} (\code{TRUE},\code{FALSE}).
}

\usage{
\method{[}{comment}(x, ..., drop = TRUE)
\method{[[}{comment}(x, ..., drop = TRUE)
\method{as.comment}{default}(x, ...)
\method{as.data.frame}{comment}(x, row.names = NULL, optional = FALSE, ...)
\method{c}{comment}(..., recursive = FALSE)
\method{format}{comment}(x, ...)
\method{hide}{data.frame}(x, where, why, ...)
\method{print}{comment}(x, ...)
\method{rep}{comment}(x, ...)
\method{hidden}{data.frame}(x, ...)
\method{summary}{hidden.data.frame}(object, ...)
}

\arguments{
  \item{x}{the comment object, or something coercible to logical 
        (data.frame for \code{hidden.data.frame})}
  \item{\dots}{extra arguments, generally ignored}
  \item{drop}{coerce to lowest possible dimension}
  \item{row.names}{row names for the result}
  \item{optional}{Optional to use the object name as the column name?}
  \item{recursive}{unused, present for historical reasons}
  \item{where}{logical with length \code{nrow(x)}, indicating rows to hide}
  \item{why}{a column name suggesting the reason for hiding}
  \item{object}{hidden.data.frame}
}

\details{
Objects of class comment can be used exactly like logicals, but look like
conventional comments.  \code{hide.data.frame} implements consistent
commenting of rows, including a reason for the comment. \code{hidden} and 
\code{summary} methods analyze the result.  \code{hidden.data.frame} returns
just the hidden rows, classified as \sQuote{hidden.data.frame}.  
\code{summary.hidden.data.frame} indicates, for each flag, the number
of records commented for that reason (total) and the number of records
commented for only that reason (unique), if any. If \code{hidden(x)} has
no hide flags, the result is a data frame with one cell: ncol=0. If 
\code{hidden(x)} has no rows, the result is a data frame with one cell: nrow=0.
}
\value{comment, or data.frame for \code{hide.data.frame}, etc.}
\examples{
Theoph <- hide(Theoph, where=Theoph$conc < 1,why='blq')
Theoph <- hide(Theoph, where=Theoph$Subject==1,why='suspect')
hidden(Theoph)
summary(hidden(Theoph))
}
\keyword{manip}


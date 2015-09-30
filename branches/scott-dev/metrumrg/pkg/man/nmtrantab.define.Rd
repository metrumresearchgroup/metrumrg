\name{nmtrantab.define}
\alias{nmtrantab.define}
\alias{nmtrantab.define.spec}
\title{Work with data specifications}
\description{
Produce a report table of the contents of an NMTRAN data file.
}
\usage{
\method{nmtrantab.define}{define}(x,sep=' : ',collapse='\n\n',title,source,source.label,file,file.label)
}
\arguments{
  \item{x}{a \code{specification} object.}
  \item{sep}{separates codes from respective decodes where given}
  \item{collapse}{separates code/decode pairs where given}
  \item{title}{Title for the header row of the table.}
  \item{source}{Passed to \code{tabular.data.frame}. Location and name of the script that created the table.}
  \item{source.label}{Passed to \code{tabular.data.frame}. Provides a label for \code{source}.}
  \item{file}{Passed to \code{tabular.data.frame}. Where to write the file. Will also be included as a footnote to the table.}
  \item{file.label}{Passed to \code{tabular.data.frame}. Provides a label for \code{file}.}
  
}
\value{
Creates a latex table with columns: "Variable","Label","Codes". Populated from \code{specification}.
}
\references{\url{http://metrumrg.googlecode.com}}
\author{Scott Pivirotto}
\seealso{
  \code{\link{specification}}, 
	\code{\link{as.spec}}
	\code{\link{tabular.define}}
}
\examples{
## Not run ##
y <- nmtrantab.define(spec,sep=' : ',
                        collapse='\n\n',
                        title='NMTRAN Analysis Data Items',
                        source='/script/nmtrantab.tex',
                        source.label='Source Code: ',
                        file='/script/nmtrantab.tex',
                        file.label='Source Tex: '
)
}
\keyword{manip}


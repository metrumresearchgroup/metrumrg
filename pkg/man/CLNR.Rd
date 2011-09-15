\name{CLNR}
\alias{CLNR}
\title{Delete Files and Directories}
\description{
  CLNR deletes files and directories and logs what was deleted 
  to a text file in \file{project}.
}
\usage{
CLNR(Dir, project, note = 'Files removed', test = TRUE)
}
\arguments{
  \item{Dir}{Complete path of directory to be deleted.  This directory
   and all files and sub-directories will be deleted. 
   Example: \code{Dir='/Dir/to/be/Removed'}}
  \item{project}{System path to the directory containing the NONMEM control (*.ctl) streams.}
  \item{note}{character vector (defaults to 'Files removed') for user 
  text to describe why something was deleted. Example: \code{note=c('Files removed ')}}
  \item{test}{logical to determine if you want to actually 
  	  delete the files or just see what files would be deleted. To delete directory, files, and
  sub-directories use \code{test=F}.}
}
\references{\url{http://mifuns.googlecode.com}}
\author{Bill Knebel}
\keyword{manip}

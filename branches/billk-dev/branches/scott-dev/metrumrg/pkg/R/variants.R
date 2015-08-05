`variants` <-
function(x,path){
  if(!length(x))stop('length x must be 1 or more')
  if(!is.character(x))stop('x must be character')
  if(!is.character(path))stop('path must be character')
  if(!length(path) %in% c(1,length(x)))stop('path must be scalar or length(x)')  
  dirs <- file.path(path,x)
  subdirs <- lapply(dirs,dir,full.names=TRUE)
  counts <- sapply(subdirs,length)
  files <- file.path(unlist(subdirs),rep(x,counts))
  missing <- !file_test('-f',files)
  if(any(missing))warning(paste('nonexistent file(s), e.g.',files[missing][[1]]))
  files  
}


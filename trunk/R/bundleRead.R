## -----------------------------------------------------------------
## read in files from directories into a list
bundleRead <- function(dir, ext, func, ...)
{
  func <- ifelse(is.list(func),func,list(func))
  x <- lapply(dir,function(y)list.files(y))
  x <- lapply(x,function(y){
    keep <- NULL
    for(i in 1:length(ext)) keep <- c(keep,grep(ext[i],y))
    y[order(keep)]})
  n <- strsplit(unlist(x),paste(".",ext,sep=""))
  for(j in 1:length(dir)) x[[j]] <- file.path(dir[j],x[[j]])
  x <- as.list(unlist(x))
  x <- lapply(x, function(y){ 
    for(k in 1:length(ext)) if(grepl(ext[k],y)) z <- func[[k]](y)
    return(z)})
  names(x) <- n
  return(x)
}
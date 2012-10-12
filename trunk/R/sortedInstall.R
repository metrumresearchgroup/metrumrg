sortedInstall <- function(pkgDir=getwd(),
                          libDir=NULL,
                          addLib=TRUE, 
                          tmpName='tmpdir')
{
  f <- list.files(pkgDir)
  f <- file.path(pkgDir, f[grepl('tar.gz',f)])
  tmp <- file.path(pkgDir, tmpName)
  dir.create(tmp)
  for(i in f) system(paste('tar -xzf', i, '-C', tmp))
  p <- list.files(tmp)
  d <- unlist(lapply(p,function(x)file.path(tmp, x,'DESCRIPTION')))
  dl <- NULL
  for(j in 1:length(d)){
    r <- readLines(d[j])
    l <- unlist(strsplit(r[grep('Depends:',r)], " "))
    if(length(l)>0) l <- unlist(strsplit(l,","))
    dep <- intersect(p,l)
    if(length(dep)>0){
      if(length(dl)<=1) dl <- c(dep,dl)
      else if(length(dl)>1){
        if(p[j]%in%dl){
          ind <- min(grep(p[j],dl))
          if(ind==1) dl <- c(dep, dl)
          else if (ind!=1) dl <- c(dl[1:(ind-1)], dep, dl[ind:length(dl)])
        }
        else if(!p[j]%in%dl) dl <- c(dl,dep)
      }
    }
  }
  unlink(tmp, recursive=TRUE, force=TRUE)
  il <- unique(c(dl,p))
  n <- NULL
  for(pkg in il){
    v <- f[grep(pkg,f)]
    if(length(v)==1) n <- c(n,v)
    if(length(v)>1) {
      print(paste("There is more than 1", pkg, "source. Please select one to install:"))
      n <- c(n,select.list(v))
    }
  }
  if(is.null(libDir)) libDir <- pkgDir
  install.packages(pkgs=n, lib=libDir, repos = NULL, type = 'source')
  if(addLib==TRUE) .libPaths(libDir)
}

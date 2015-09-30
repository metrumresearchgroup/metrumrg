simpar <-
function(nsim,theta,covar,omega,sigma,odf=NULL,sdf=NULL,digits=4,min=-Inf,max=Inf){
  covar <- as.matrix(covar)
  if(ord(covar)!=length(theta))stop('order of covar is not equal to length theta')
  if(det(covar) < 0) stop("covar is not positive-definite.")
  if(!is.list(omega)) omega <- list(omega)
  if(!is.list(sigma)) sigma <- list(sigma)
  omega <- lapply(omega,as.matrix)
  sigma <- lapply(sigma,as.matrix)
  if(is.null(odf))odf <- sapply(omega,length)
  if(is.null(sdf))sdf <- sapply(sigma,length)
  npar <- length(theta) + sum(sapply(omega,function(x)length(half(x)))) + sum(sapply(sigma,function(x)length(half(x))))
  if(length(min)==1) min <- rep(min,npar)
  if(length(max)==1) max <- rep(max,npar)
  if(length(min)!=npar)min <- c(min, rep(-Inf,npar-length(min)))
  if(length(max)!=npar)max <- c(max, rep( Inf,npar-length(max)))
  if(length(max)!=npar)stop('length of max should be one, or number of parameters')
  if(length(min)!=npar)stop('length of min should be one, or number of parameters')
  if(!all(sapply(omega,is.square)))stop('not all omega blocks are square')
  if(!all(sapply(sigma,is.square)))stop('not all sigma blocks are square')
  if( any(sapply(omega,function(x)det(x)<0)))stop('not all omega blocks are positive-definite')
  if( any(sapply(sigma,function(x)det(x)<0)))stop('not all sigma blocks are positive-definite')
  if(length(odf)!=length(omega))stop('length odf differs from length omega')
  if(length(sdf)!=length(sigma))stop('length sdf differs from length sigma')
  if(any(odf < sapply(omega,length)))stop('odf[n] is less than number of elements in corresponding matrix')
  if(any(sdf < sapply(sigma,length)))stop('sdf[n] is less than number of elements in corresponding matrix')
  mvr <- mvrnorm(nsim,theta,covar)
  if(nsim==1)mvr <- t(as.matrix(mvr))
  omg <- lapply(1:length(odf),function(x)list(n=nsim,df=odf[[x]],cov=omega[[x]]))
  sig <- lapply(1:length(sdf),function(x)list(n=nsim,df=sdf[[x]],cov=sigma[[x]]))
  omg <- do.call(cbind,lapply(omg,function(x)do.call(simblock,x)))
  sig <- do.call(cbind,lapply(sig,function(x)do.call(simblock,x)))
  dimnames(mvr) <- dimnames(omg) <- dimnames(sig) <- list(NULL,NULL)
  dimnames(mvr)[[2]] <- paste('TH',seq(length.out=dim(mvr)[[2]]),sep='.')
  dimnames(mvr)[[1]] <- seq(length.out=dim(mvr)[[1]])
  impliedNames <- function(x){#converts a vector of block sizes to implied full-block names
	   vars <- sum(x)
	   crit <- cumsum(x)-x+1 
	   diag <- diag(rep(crit,x))
	   good <- outer(colSums(diag),rowSums(diag),FUN='==')
	   half <- half(good)
	   names(half[half])
  }
  dimnames(omg)[[2]] <- glue('OM',impliedNames(sapply(omega,ord)))
  dimnames(omg)[[1]] <- seq(length.out=dim(omg)[[1]])
  dimnames(sig)[[2]] <- glue('SG',impliedNames(sapply(sigma,ord)))
  dimnames(sig)[[1]] <- seq(length.out=dim(sig)[[1]])
  sim <- cbind(mvr,omg,sig)
  sim <- round(signif(sim,digits),6)
  sim <- sim[apply(t(t(sim)-min)>=0,MARGIN=1,all),,drop=FALSE]
  if(dim(sim)[[1]]==0){
  	  warning(nsim,' of ',nsim,' rows dropped',call.=FALSE,immediate.=TRUE)
  	  return(sim)
  }
  sim <- sim[apply(t(t(sim)-max)<=0,MARGIN=1,all),,drop=FALSE]
  loss <- nsim - nrow(sim)
  if(loss)warning(loss,' of ',nsim,' rows dropped',call.=FALSE,immediate.=TRUE)
  sim
}

is.square <- function(x,...)UseMethod('is.square')
is.square.matrix <- function(x,...)dim(x)[[1]]==dim(x)[[2]]
ord <- function(x,...)UseMethod('ord')
ord.matrix <- function(x,...){
	if(!is.square(x))stop('matrix is not square')
	dim(x)[[1]]
}
rinvchisq <- function(n,df,cov)df*cov/rchisq(n, df)
simblock <- function(n,df,cov){
    if(df < length(cov))stop('df is less than matrix length')
    if(length(cov)==1)return(rinvchisq(n,df,cov))
    s <- dim(cov)[1]
    ncols <- s*(s+1)/2
    res <- matrix(nrow=n, ncol=ncols)
    for(i in 1:n)res[i,] <- half(posmat(riwish(s,df-s+1,df*cov)))
    res
}
riwish <- function(s,df,prec){
   if (df<=0) stop ("Inverse Wishart algorithm requires df>0")
   R <- diag(sqrt(2*rgamma(s,(df + s  - 1:s)/2)))
   R[outer(1:s, 1:s,  "<")] <- rnorm (s*(s-1)/2)
   S <- t(solve(R))%*% chol(prec)
   t(S)%*%S
}
half <- function(x,...)UseMethod('half')
half.matrix <- function(x,...) {
    if(!isSymmetric(x))stop('matrix is not symmetric')
    d <- ord(x)
    dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
    dex <- dex[dex$col <= dex$row,]
    x <- x[as.matrix(dex)]
    names(x) <- do.call(paste,c(dex,list(sep='.')))
    structure(x,class='halfmatrix')
}
ord.halfmatrix <- function(x,...){
	ord <- sqrt(0.25+2*length(x))-0.5
	if(as.integer(ord)!=ord)stop('invalid length for half matrix')
	ord
}
print.halfmatrix <- function(x,...)print(unclass(x))
as.halfmatrix <- function(x,...)UseMethod('as.halfmatrix')
as.halfmatrix.halfmatrix <- function(x,...)x
as.halfmatrix.default <- function(x,...)half(as.matrix.halfmatrix(x))
as.matrix.halfmatrix <- function(x,...){
	d <- ord.halfmatrix(x)
	y <- matrix(nrow=d,ncol=d)
	dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
    dex <- dex[dex$col <= dex$row,]
    y[as.matrix(dex)] <- x
    y[is.na(y)] <- t(y)[is.na(y)]
    y  
}
posmat <- function(x,...) {
     if(any(diag(x) <=0)) stop("matrix cannot be made positive-definite")
     if(!is.square(x))stop('x is not square')
	 sign <- sign(x)
	 x <- abs(x)
	 characteristic <- trunc(log(x,10))
	 mantissa <- log(x,10) - characteristic
	 scale <- 10^characteristic
	 digits <- 10^mantissa * 1e5
	 diagonal <- round(diag(digits))
	 digits <- floor(digits)
	 diag(digits) <- diagonal
	 digits <- digits/1e5
	 x <- sign * scale * digits
	 diagonal <- diag(x)
	 y <- 0.97 * x
	 diag(y) <- diagonal
	 if(det(x)>0) x else posmat(y)
}
offdiag <- function(x,...)UseMethod('offdiag')
offdiag.halfmatrix <- function(x,...)x[sapply(strsplit(names(x),'\\.'),`[`,1)!=sapply(strsplit(names(x),'\\.'),`[`,2)]


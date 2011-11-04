is.diagonal <-
function(x){
  dec <- text2decimal(x)
  ord <- trunc(max(dec))
  mat <- half(diag(ord))
  off <- offdiag(mat)
  nms <- setdiff(names(mat),names(off))
  was <- as.character(dec)
  is.diag <- was %in% nms
  is.diag
}
is.offdiagonal <-
function(x){
  dec <- text2decimal(x)
  ord <- trunc(max(dec))
  mat <- half(diag(ord))
  off <- offdiag(mat)
  nms <- names(off)
  is.off <- as.character(dec) %in% nms
  is.off
}
is.fixed <- function(x)contains("THETA",x,ignore.case=TRUE)
is.random <- function(x)contains("OMEGA|SIGMA",x,ignore.case=TRUE)
is.iiv <- function(x)contains("OMEGA",x,ignore.case=TRUE)
is.residual <- function(x)contains("SIGMA",x,ignore.case=TRUE)


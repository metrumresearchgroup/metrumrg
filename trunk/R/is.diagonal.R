on.diagonal <-function(x){
  dec <- text2decimal(x)
  txt <- as.character(dec)
  spt <- strsplit(txt,'.',fixed=TRUE)
  row <- sapply(spt,function(x)if(length(x)==2)as.numeric(x[[1]])else NA)
  col <- sapply(spt,function(x)if(length(x)==2)as.numeric(x[[2]])else NA)
  row==col
}
is.diagonal <- function(x) !is.na(on.diagonal(x)) & on.diagonal(x)
is.offdiagonal <- function(x) !is.na(on.diagonal(x)) & !on.diagonal(x)
is.fixed <- function(x)contains("THETA",x,ignore.case=TRUE)
is.random <- function(x)contains("OMEGA|SIGMA",x,ignore.case=TRUE)
is.iiv <- function(x)contains("OMEGA",x,ignore.case=TRUE)
is.residual <- function(x)contains("SIGMA",x,ignore.case=TRUE)


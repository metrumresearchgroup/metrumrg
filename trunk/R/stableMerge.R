`stableMerge` <-
function(x,y){
#left join with stable row count, row order, column order, and row names
#returns equiv of x with values of y appended (looked up by common columns)
colnames <- names(x)
rowNames <- rownames(x)
if(all(names(y) %in% names(x))){
warning("nothing to merge")
return(x)
}
key <- names(y)[names(y) %in% names(x)]
if (
!identical(
nrow(y[,key,drop=FALSE]),
nrow(unique(y[,key,drop=FALSE]))
)
) stop("keys in y not unique")
x$stable_Index <- 1:nrow(x)
z <- merge(x,y,all.x=TRUE,all.y=FALSE,sort=FALSE)
z <- z[order(z$stable_Index),]
z$stable_Index <- NULL
z <- z[,c(colnames,names(z)[!names(z) %in% colnames])]
rownames(z) <- rowNames
return(z)
}


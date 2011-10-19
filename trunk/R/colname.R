`colname<-` <-
function(x,value){
colnames(x) <- replace(colnames(x),match(attr(value,'names'),colnames(x)),value)
x
}
`name<-` <-
function(x,value){
names(x) <- replace(names(x),match(attr(value,'names'),names(x)),value)
x
}


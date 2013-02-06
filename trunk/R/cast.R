setGeneric('cast')
setOldClass(c('keyed','data.frame'))
setMethod(
  'cast',
  'keyed',
  function(
    data, 
    formula = ... ~ variable, 
    fun.aggregate=NULL, 
    check.names=FALSE,
    stringsAsFactors=FALSE,
    reclass=TRUE,
    ..., 
    margins=FALSE, 
    subset=TRUE, 
    df=FALSE, 
    fill=NA, 
    add.missing=FALSE, 
    value = guess_value(data)
  ){
    theClass <- class(data[[value]])
    x <- cast(
      data=as.data.frame(data),
      formula=formula,
      fun.aggregate=fun.aggregate,
      ...,
      margins=margins,
      subset=subset,
      df=df,
      fill=fill,
      add.missing=add.missing,
      value=value
    )
    key <- attr(x,'idvars')
    x <- data.frame(x, check.names=check.names,stringsAsFactors=stringsAsFactors)
    x <- as.keyed(x,key=key)
    nonKey <- x %-% key(x)
    if(reclass) x[nonKey] <- lapply(x[nonKey],function(col)structure(col,class=theClass))
    x
  }
)

melt.keyed <- function(
  data, 
  id.vars=key(data), 
  measure.vars, 
  variable_name = "variable", 
  na.rm = FALSE, 
  ...
){
  x <- melt(
    data=as.data.frame(data),
    id.vars=id.vars,
    measure.vars=measure.vars,
    variable_name=variable_name,
    na.rm=na.rm,
    ...
  )
  x <- as.keyed(x,key=setdiff(names(x),'value'))
  x
}
 
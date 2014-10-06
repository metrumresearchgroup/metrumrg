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
    value = reshape::guess_value(data)
  ){
    theClass <- class(data[[value]])[[1]]
    coerce <- glue('as.',theClass)
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
    if(reclass)if(exists(coerce,mode='function')) x[nonKey] <- lapply(x[nonKey],match.fun(coerce),...)
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
  if(missing(id.vars))message('using (key) ',paste(id.vars,collapse=', '),' as id.vars')
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
 
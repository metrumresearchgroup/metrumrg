findNonmemCommand <- function(under=c('/opt/NONMEM','/opt/nonmem'),executable='autolog.pl',...)dir(
   under,
   pattern=glue(executable,'$'),
   full.names=TRUE,
   recursive=TRUE,
   ...
)

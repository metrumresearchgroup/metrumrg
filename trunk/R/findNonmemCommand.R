findNonmemCommand <- function(under='/opt/NONMEM',executable='autolog.pl',...)dir(
   under,
   pattern=glue(executable,'$'),
   full.names=TRUE,
   recursive=TRUE,
   ...
)

nmtrantab.define <- function(
  x,
  title=' ',
  grid = FALSE,
  rules = 1,
  colwidth = c('1.0in','2.0in','3.0in'),
  tabularEnvironment = 'longtable',
  tabnum = FALSE,
  pretable = paste('\\hline \\multicolumn{3}{l}{\\textbf{',title,'}} \\\\'),
  prepos = 1,
  headerBold = TRUE,
  ...
){
  nmtrantab <- as.define(x)[,c("Variable","Label","Codes")]
  if(headerBold) names(nmtrantab) <- glue('\\textbf{',names(nmtrantab),'}')
  nmtrantab <- tabular.data.frame(
    nmtrantab,
    title=title,
    grid=grid,
    rules=rules,
    colwidth=colwidth,
    tabularEnvironment=tabularEnvironment,
    endhead='TRUE',
    ...
  )
  nmtrantab <- append(nmtrantab,pretable,prepos)
  nmtrantab
}

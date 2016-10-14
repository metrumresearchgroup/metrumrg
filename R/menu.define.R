menu.define <- function(
  dname = '',
  title,
  desc,
  grid = TRUE,
  rules = 1,
  colwidth = c('1.0in','2.5in','1.0in'),
  tabularEnvironment = 'tabu',
  walls = 1,
  tabnum = FALSE,
  pretable = paste('\\hline \\multicolumn{3}{|l|}{\\textbf{',title,'}} \\\\'),
  prepos = 1,
  headerBold = TRUE,
  ...
){
  menu <- data.frame(Dataset=paste('\\hyperlink{',dname,'}{',dname,'}',sep=''),
                     Description=desc,
                     Location=paste('\\href{run:./',dname,'.xpt}{',dname,'.xpt}',sep=''))
  if(headerBold) names(menu) <- glue('\\textbf{',names(menu),'}')
  menu <- tabular.data.frame(
    menu,
    title=title,
    dname=dname,
    desc=desc,
    grid=grid,
    rules=rules,
    colwidth=colwidth,
    tabularEnvironment=tabularEnvironment,
    walls=walls,
    ...
  )
  menu <- append(menu,pretable,prepos)
  menu
}

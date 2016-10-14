tabular.data.frame <- function (
  x, 
  rules = c(2, 1, 1), 
  walls = 0, 
  grid = FALSE, 
  rowgroups = rownames(x), 
  colgroups = names(x), 
  rowbreaks = if (grid) breaks(rowgroups,...) else 0, 
  colbreaks = if (grid) breaks(colgroups,...) else 0, 
  rowcolors = NULL, 
  charjust = "left", 
  numjust = "right", 
  justify = ifelse(sapply(x, is.numeric), numjust, charjust), 
  colwidth = NA, 
  paralign = "top", 
  na = "", 
  verbatim = ifelse(sapply(x,is.numeric), TRUE, FALSE), 
  escape = "#", 
  trim = TRUE, 
  source = NULL, 
  file = NULL, 
  source.label = "source: ", 
  file.label = "file: ",
  basefile = FALSE, 
  tabularEnvironment = "tabular", 
  endhead=FALSE,...
  ) 
{
  x <- as.data.frame(x)
  rules <- rep(rules, length.out = 3)
  walls <- rep(walls, length.out = 2)
  rowgroups <- rep(rowgroups, length.out = nrow(x))
  colgroups <- rep(colgroups, length.out = ncol(x))
  rowbreaks <- rep(rowbreaks, length.out = nrow(x) - 1)
  colbreaks <- rep(colbreaks, length.out = ncol(x) - 1)
  if (!is.null(rowcolors)) 
    rowcolors <- rep(rowcolors, length.out = nrow(x))
  stopifnot(length(charjust) == 1)
  stopifnot(length(numjust) == 1)
  stopifnot(length(escape) == 1)
  stopifnot(charjust %in% c("left", "right", "center"))
  stopifnot(numjust %in% c("left", "right", "center"))
  na <- rep(na, length.out = ncol(x))
  verbatim <- as.logical(rep(verbatim, length.out = ncol(x)))
  paralign <- map(paralign, from = c("top", "middle", "bottom"), 
                  to = c("p", "m", "b"))[[1]]
  colwidth <- rep(colwidth, length.out = ncol(x))
  colwidth <- sub("^", glue(paralign, "{"), colwidth)
  colwidth <- sub("$", "}", colwidth)
  justify <- rep(justify, length.out = ncol(x))
  decimal <- justify == "decimal"
  justify <- map(justify, from = c("left", "right", "center", 
                                   "decimal"), to = c("l", "r", "c", "r"))
  justify[!is.na(colwidth)] <- colwidth[!is.na(colwidth)]
  format <- tabularformat(justify = justify, breaks = colbreaks, 
                          walls = walls)
  header <- row2tabular(names(x)) 
  if (endhead) header <- paste(header,'\\hline \\endhead')
  sapply(names(x)[verbatim], function(nm) if (any(!is.na(x[[nm]]) & 
                                                    contains(escape, x[[nm]], fixed = TRUE))) 
    warning(nm, "contains", escape))
  x[] <- lapply(seq_along(x), function(col) if (decimal[[col]]) 
    align.decimal(x[[col]], ...)
    else format(x[[col]], trim = trim, ...))
  x[] <- lapply(seq_along(x), function(col) sub("^ *NA *$", 
                                                na[[col]], x[[col]]))
  x[] <- lapply(seq_along(x), function(col) if (verbatim[[col]]) 
    glue("\\verb", escape, x[[col]], escape)
    else x[[col]])
  x <- as.matrix(x)
  x <- apply(x, 1, row2tabular)
  if (!is.null(rowcolors)) 
    x <- glue("\\rowcolor{", rowcolors, "}", x)
  x <- c("", header, x)
  rowbreaks <- c(rules[1:2], rowbreaks, rules[[3]])
  stopifnot(length(rowbreaks) == length(x))
  while (any(rowbreaks > 0)) {
    x[rowbreaks > 0] <- paste(x[rowbreaks > 0], "\\hline")
    rowbreaks <- rowbreaks - 1
  }
  x <- wrap(x, tabularEnvironment, args = format)
  class(x) <- c("tabular", class(x))
  if (!is.null(source)) 
    if (!is.null(source.label)) 
      x <- c(x, glue("{\\raggedleft \\tiny ", source.label, source, 
                     "}"))
  if (!is.null(file)) 
    if (!is.null(file.label)) 
      x <- c(x, glue("\\\\ {\\raggedleft \\tiny ", file.label, if (basefile) basename(file) else file, 
                     "}"))
  if (is.null(file)) 
    return(x)
  else {
    writeLines(x, file)
    invisible(x)
  }
}

`panel.stratify` <-
function(
	x, 
	y, 
	type = "p", 
	groups = NULL, 
	pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch, 
    col, 
    col.line = if (is.null(groups)) plot.line$col else superpose.line$col, 
    col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col, 
    font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font, 
    fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily, 
    fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface, 
    lty = if (is.null(groups)) plot.line$lty else superpose.line$lty, 
    cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex, 
    fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill, 
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd, 
    horizontal = FALSE, 
    panel.levels='panel.xyplot',
    ..., 
    jitter.x = FALSE, 
    jitter.y = FALSE, 
    factor = 0.5, 
    amount = NULL
){
    if (all(is.na(x) | is.na(y))) 
        return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
        if (missing(col.symbol)) 
            col.symbol <- col
    }
    if (!is.null(groups)) 
        panel.superpose(x, y, type = type, groups = groups, pch = pch, 
            col.line = col.line, col.symbol = col.symbol, font = font, 
            fontfamily = fontfamily, fontface = fontface, lty = lty, 
            cex = cex, fill = fill, lwd = lwd,horizontal = horizontal, 
            panel.groups = panel.stratify, jitter.x = jitter.x, 
            jitter.y = jitter.y, factor = factor, amount = amount,
            panel.levels=panel.levels,...
       )
       else{
			levels <- if(horizontal) y else x
			x <- split(x,levels)
			y <- split(y,levels)
			panel.levels <- if (is.function(panel.levels)) panel.levels
    			else if (is.character(panel.levels)) get(panel.levels)
    			else eval(panel.levels)
    		for (level in unique(levels)) {
    			panel.levels(
    					x[[as.character(level)]], 
					y[[as.character(level)]], 
					type = type, 
					pch = pch, 
    					col = col, 
    					col.line = col.line, 
    					col.symbol = col.symbol, 
    					font = font, 
    					fontfamily = fontfamily, 
    					fontface = fontface, 
    					lty = lty, 
    					cex = cex, 
    					fill = fill, 
    					lwd = lwd,
    					horizontal = horizontal, 
    					jitter.x = jitter.x,
    					jitter.y = jitter.y,
    					factor = factor,
    					amount = amount,
    					level=level,
    					levels=levels,
    					...
    			)
			}
		}
}


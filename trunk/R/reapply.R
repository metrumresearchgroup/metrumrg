reapply <- 
function (x, INDEX, FUN, ...) 
{
	if(!is.list(INDEX)) INDEX <- list(INDEX)
	INDEX <- lapply(INDEX,function(x)as.integer(factor(x)))
	INDEX <- as.integer(do.call(interaction,c(INDEX,drop=TRUE)))
	form <- tapply(x, INDEX)
	calc <- tapply(x, INDEX, FUN, ...,simplify=FALSE)
	need <- table(form)
	calc <- lapply(
		seq_along(calc),
		function(cell)rep(
			calc[[cell]],
			length.out=need[[
				as.character(cell)
			]]
		)
	)
	calc <- c(calc,list(rep(NA,sum(is.na(form)))))
	form[is.na(form)] <- length(calc)
	grps <- split(form,form)
	grps <- lapply(
		grps, 
		seq_along
	)
	elem <- unsplit(grps,form)
	sapply(
		seq_along(form),
		function(i)calc[[
			form[[i]]
		]][[
			elem[[i]]
		]]
	)
}



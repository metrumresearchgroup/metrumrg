`bracket` <-
function(x,close=FALSE,...)sub(
	'(^</[^ ]+) .+>$',
	'\\1>',
	do.call(
		paste,
		c(
			list(
				sep='',
				'<',
				ifelse(close, '/' , ''),
				x
			),
			lapply(names(list(...)),function(x)attribute(list(...)[[x]],tag=x)),
			list(
				'>'
			)
		)
	)
)

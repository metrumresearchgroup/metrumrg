safeQuote <- function(x){
	hasSpace <- contains(" ", x)
	isQuoted <- contains("^[\"']", x)
	need <- hasSpace & !isQuoted
	x[need] <- glue("'", x[need], "'")
	x
}

    
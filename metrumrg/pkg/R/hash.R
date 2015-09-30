`hash` <-
function(x,char='#'){
	con <- file()
	sink(con)
	result <- try(x)
	if(!inherits(result,"try-error"))print(result)
	comments <- paste(char,readLines(con))
	sink(NULL)
	close(con)
	writeLines(comments)	
}
naInContext <- function(x,context,search){
	guilty <- !complete.cases(x[,search])
	keys <- interaction(x[,context])
	x[keys %in% keys[guilty],]
}

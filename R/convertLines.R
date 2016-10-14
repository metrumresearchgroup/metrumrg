convertLines <- function(source,destination=source,...){
	txt <- readLines(con=source,...)
	writeLines(text=txt,con=destination,...)
}

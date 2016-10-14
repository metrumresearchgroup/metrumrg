`check.subjects` <-
function(x,subject){
   columnNames <- names(x)
   print(paste(length(x[,subject]),"records from",length(unique(x[,subject])),"subjects"))
   for(columnName in columnNames) {
     if( any(is.na(x[,columnName]))) {
       temp <- x[,c(subject,columnName)]
       names(temp) <- c("ID","value")
       nsubAll <- length(unique(temp$ID))
       nsubNA <- length(unique(temp$ID[is.na(temp$value)]))
       nsub   <- length(unique(temp$ID[!is.na(temp$value)]))
       if(nsubAll == nsub + nsubNA){
          print(paste(columnName,": ",nsubNA, " (",round(100*nsubNA/nsub,1),
             "%) of subjects have only missing values",sep=""))
       }else{
        print(glue(columnName,": ",nsubAll-nsubNA," (",
                 round(100*(nsubAll-nsubNA)/nsubAll,1),"%) of subjects have all x;"))
        print(glue("       ",nsubAll-nsub," (",round(100*(nsubAll-nsub)/nsubAll,1),
             "%) of subjects have only missing values;"))
        print(glue("       ",nsubNA+nsub -nsubAll," (",round(100*(nsubNA+nsub -nsubAll)/nsubAll,1),
                 "%) of subjects have some missing values"))
       }  
     }
   }

   for(columnName in columnNames) {
     temp <- x[,columnName]
     temp <- temp[!is.na(temp)]
     if( any(is.na(as.double(as.character(temp))))) {
       print(paste(columnName,"is not a numeric variable"))
       if(length(unique(temp)) < 10) {
          print(paste(columnName,"levels:",paste(sort(unique(temp)),collapse=",")))
        }
     }else{
        if(length(unique(temp)) < 10) {
          print(paste(columnName,"levels:",paste(sort(unique(temp)),collapse=",")))
        }else{
           print(paste(columnName,"range:",range(temp)[1],"-",range(temp)[2])) 
        }  
     }
   }

   columnNames <- columnNames[columnNames != subject]
   for(columnName in columnNames) {
       temp <- x[,c(subject,columnName)]
       names(temp) <- c("ID","value")
       temp <- temp[!is.na(temp$value),]
       if(length(unique(temp$ID)) != length(unique(paste(temp$ID,temp$value))) ) {
          print(paste(columnName,": vary with time within some subjects"))
       } else {
                print(paste(columnName,": constant within all subjects"))       
       }
   }
}


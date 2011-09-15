epilog <- function(run,project,...){

    #See R help. dataSynthesis() gives the same dataset that is used by PLOTR.
    data <- dataSynthesis(run=run,project=project,...)
    
    numDV<-intersect(c('DV','LDV','AMT'),names(data)) 
    # loop to change variables in numDV to numeric
    for(k in numDV) data[[k]]<-as.numeric(as.character(data[[k]]))
    subj<-data[!duplicated(data$ID),]

    # user supplied name of output pdf file
    pdf(
    	file=star(
		filename(
			project,
			file.path(
				'*',
				'TestPlots'
			),
			'.pdf'
		),
		run
	)
    )

    #The plotting code below can be as complicated or as simple as is required. 

    # plot of observed PK vs Time based on Nonmem data set
    plot(data$TIME, data$DV, main='PK obs', xlab='Time', ylab='PK measure')
     
    # histogram of ETA1 based on Nonmem table file (XXpar.TAB)       
    hist(subj$ETA1, main='Histogram of ETA1')

    dev.off()
}

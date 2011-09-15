rig <- function(x,...)UseMethod('rig')
rig.nm <- function(x,n=50,dateTime=FALSE,noDups=FALSE,...){
	y <- x
	x <- x[!x$C,]
	if('EVID' %in% names(x)) x <- x[x$ID %in% x$ID[x$EVID==1],]
	if('DATETIME' %in% names(x))if(!constant(is.na(x$DATETIME),within=x$ID))stop(
		'mixed HOUR/DATETIME within ID'
	)
	if(dateTime  & !'DATETIME' %in% names(x))x <- x[0,]
	if(dateTime  &  'DATETIME' %in% names(x))x <- x[!is.na(x$DATETIME),]
	if(!dateTime &  'DATETIME' %in% names(x))x<- x[is.na(x$DATETIME),]
	#if(!dateTime & !'DATETIME' %in% names(x))x<- x
	if(nrow(x)){
		if(dateTime)x <- do.call(
    		rbind,
    		by(
    			x,
    			x$ID,
    			function(x)data.frame(
    				SUBJ=rep(x$SUBJ[[1]],n),
    				DATETIME=as.miDateTime(
    					round(
    						seq(
    							from=min(x$DATETIME),
    							to=max(x$DATETIME),
    							length.out=n
    						)/60
    					)*60
    				)
    			)
    		)
    	)     	
    	else x <- do.call(
			rbind,
        	by(
        		x,
        		x$ID,
        		function(x)data.frame(
        			SUBJ=rep(
        				x$SUBJ[[1]],n
        			),
        			HOUR=seq(
        				from=min(x$HOUR), 
       					to=max(x$HOUR),
       					length.out=n
       				)
       			)
    		)
    	)	
    	x <- as.keyed(x,key=names(x))
    	dups <- do.call(paste,x) %in% do.call(paste,y[,names(x)])
    	if(noDups) x <- x[!dups,]
    }  
    x
} 

as.rigged <- function(...){
	x <- list(...)
	class(x) <- c('rigged','keyed')
	x
}

`plus.rigged` <- function (x, y){
	a <- do.call(rig,c(list(x=x),y))
	b <- do.call(rig,c(list(x=x,dateTime=TRUE),y))
	if(nrow(a)) x <- x + as.keyed(transform(a,EVID=2),key='')
	if(nrow(b)) x <- x + as.keyed(transform(b,EVID=2),key='')
	x
}

`moot` <- function(x,...)UseMethod("moot")

`moot.nm` <- function(x,scope=x$EVID==2,disregard=c("TIME","DATETIME","DATE","TAFD","TAD","HOUR"),...){
	y <- x[,names(x)[!names(x) %in% disregard]]#cols with potentially relevant changes
	target <- do.call(paste,y)#one super col
	suspect <- duplicated(target,fromLast=TRUE)#could be unnecessary
	moot <- suspect & scope#is unnecessary
	return (moot)	
}

as.moot <- function(...){
	x <- list(...)
	class(x) <- c('moot','keyed')
	x
}

`minus.moot` <- function(x,y,...){
	moot <- do.call(moot,c(list(x=x),y))
	writeLines(
		paste(
			'dropping     ',
			sum(moot),
			'of',
			nrow(x),
			'rows'
		)
	)		
	x[!moot,]
}

`deranged` <- function(x,...)UseMethod("deranged")

`deranged.data.frame` <- function(x,start,stop,result=start,dropStop=TRUE,...){
	y <- do.call(
		rbind,
		lapply(
			split(x,1:nrow(x)),
			function(x,...){
				instances <- seq(from=x[[start]],to=x[[stop]])
				x <- x[rep(1,length(instances)),]
				x[[result]] <- instances
				if(dropStop)x[[stop]] <- NULL
				x			
			}
		)
	)
	rownames(y) <- NULL
	return(y)
}

`deranged.keyed` <- function(x,...){
	y <- NextMethod()
	key(y) <- key(x)
	class(y) <- class(x)
	y
}



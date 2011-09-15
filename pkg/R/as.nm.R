read.nm <- function(
	x,
	na.strings='.',
	as.is=TRUE,
	key=c('SUBJ','TIME','SEQ'),
	flags=character(0),
	...
){
	tran <- as.keyed(
		read.csv(
			x,
			na.strings=na.strings,
			as.is=as.is,
			...
		),
		key=key
	)
	if('C' %in% names(tran))tran$C <- as.comment(!is.na(tran$C))
	if('SEQ' %in% names(tran))tran$SEQ <- as.flag(as.numeric(tran$SEQ))
	if('ID' %in% names(tran))tran$ID <- as.numeric(tran$ID)
	if('MDV' %in% names(tran))tran$MDV <- as.flag(as.numeric(tran$MDV))
	if(any(naKeys(tran) & !tran$C))warning('file has na Keys')
	if(any(dupKeys(tran)))warning('file has duplicate keys')
	#if(!is.integer(tran$ID))warning('ID is not integer')
	if('DATETIME' %in% names(tran))tran$DATETIME <- as.miDateTime(tran$DATETIME)
	for(f in flags)tran[[f]] <- as.flag(as.numeric(tran[[f]]))
	class(tran) <- c('nm',class(tran))
	tran
}
naKeys.nm <- function(x,...){
	res <- !x$C #if records are commented, they're not na keys
	res[!x$C] <- naKeys.default(x[!x$C,]) #if not commented, then evaluated
	res

}

dupKeys.nm <- function(x,...){
	res <- !x$C #if records are commented, they're not dup keys
	res[!x$C] <- dupKeys.default(x[!x$C,]) #if not commented, then evaluated
	res

}

badDv <- function(x,...)UseMethod('badDv')
badDv.nm <- function(x,...){
	if(!all(c('DV','EVID') %in% names(x)))return(rep(FALSE,nrow(x)))
	with(x,is.na(DV) & EVID==0 & !C)
}

falseDv <- function(x,...)UseMethod('falseDv')
falseDv.nm <- function(x,...){
	if(!all(c('DV','EVID') %in% names(x)))return(rep(FALSE,nrow(x)))
	with(x,!is.na(DV) & EVID!=0 & !C)
}

zeroDv <- function(x,...)UseMethod('zeroDv')
zeroDv.nm <- function(x,...){
	if(!all(c('DV','EVID') %in% names(x)))return(rep(FALSE,nrow(x)))
	with(x,!is.na(DV) & EVID==0 & DV==0 & !C)
}
predoseDv <- function(x,...)UseMethod('predoseDv')
predoseDv.nm <- function(x,...){
	if(!all(c('DV','EVID') %in% names(x)))return(rep(FALSE,nrow(x)))
	relevant <- with(x,before(EVID %in% c(1,4) & !C,within=SUBJ)) #NA if condition never matches (no doses)
	relevant[is.na(relevant)] <- TRUE # if no doses, DV is predose.
	!is.na(x$DV) & relevant & !x$C

}

badAmt <- function(x,...)UseMethod('badAmt')
badAmt.nm <- function(x,...){
	if(!all(c('AMT','EVID') %in% names(x)))return(rep(FALSE,nrow(x)))
	with(x,is.na(AMT) & EVID==1 %in% c(1,4) & !C)
}

falseAmt <- function(x,...)UseMethod('falseAmt')
falseAmt.nm <- function(x,...){
	if(!all(c('AMT','EVID') %in% names(x)))return(rep(FALSE,nrow(x)))
	with(x,!is.na(AMT) & !EVID %in% c(1,4) & !C)
}

zeroAmt <- function(x,...)UseMethod('zeroAmt')
zeroAmt.nm <- function(x,...){
	if(!all(c('AMT','EVID') %in% names(x)))return(rep(FALSE,nrow(x)))
	with(x,!is.na(AMT) & AMT==0 & EVID %in% c(1,4) & !C)
}

noPk <- function(x,...)UseMethod('noPk')
noPk.nm <- function(x,...){
	if(!'EVID' %in% names(x))return(rep(FALSE,nrow(x)))
	with(x,is.na(first(where=EVID==0 & !C,within=SUBJ)))
}

badII <- function(x,...)UseMethod('badII')
badII.nm <- function (x, ...){
    if (!all(c("ADDL", "II") %in% names(x))) return(rep(FALSE, nrow(x)))
    goodADDL <- FALSE
    goodSS <- FALSE
    goodII <- FALSE
    if('ADDL' %in% names(x)) goodADDL <- with(x,!is.na(ADDL) & ADDL > 0 & !C)
    if('SS'   %in% names(x))   goodSS <- with(x,  !is.na(SS) &   SS > 0 & !C)
    if('II'   %in% names(x))   goodII <- with(x,  !is.na(II) &   II > 0 & !C)
    goodII & !(goodADDL | goodSS)
}

`summary.nm` <- function(object,by=NULL,...){
	#FUN <- function(x,y)data.frame(item=x,value=y,stringsAsFactors=FALSE)
	x<-object
	z <- list()
	z['rows'] <- nrow(x)
	z['records'] <- sum(!x$C)
	z['comments'] <- sum(x$C)
	x <- x[!x$C,]
	z['subjects'] <- length(unique(x$ID))
	z['longestCase'] <- round(max(tapply(x$TIME,x$ID,function(x,...)max(x)-min(x))))
	for(test in c(
		'naKeys',
		'dupKeys',
		'badDv',
		'falseDv',
		'zeroDv',
		'predoseDv',
		'badAmt',
		'falseAmt',
		'zeroAmt',
		'noPk',
		'badII'
	))z[test] <- sum(match.fun(test)(object)) # argument now has comments: diagnostics must support.
	if(length(by))z <- c(z,list(table=table(x[,by,drop=FALSE])))
	class(z) <- 'nm.summary'
	z	
}

`print.nm.summary` <- function(x,...){
	print(data.frame(value=unlist(x[!sapply(x,inherits,'table')])))
	writeLines('')
	lapply(
		x[sapply(x,inherits,'table')],
		print
	)
}


`write.nm` <- function(
	x,
	file,
	na='.',
	row.names=FALSE,
	quote=FALSE,
	...
	)write.csv(
	x=x,
	file=file,
	na=na,
	row.names=row.names,
	quote=quote,
	...
)

merge.nm <- function(x,y,...)as.nm(merge(data.frame(x),y,...))

`nm` <- function()as.nm(
	data.frame(
		SUBJ=vector('character'),
		HOUR=vector('numeric')
	)
)
`as.nm` <- function(x,...)UseMethod('as.nm')
`as.nm.data.frame` <- function(x,...){		
	#SUBJ must be present and defined, even for commented records.
	if(!'SUBJ' %in% names(x)) stop('nm candidate must have SUBJ')
	if(any(is.na(x$SUBJ))) stop('SUBJ cannot contain NA')
	#ID
	#SUBJ is known to be present and fully defined.
	#SUBJ and ID must have the same sort order.
	#The default levels of factor() give ID corresponding to sorted SUBJ.
	x$ID <- as.numeric(factor(x$SUBJ))
	
	#Comment will be imputed if not present.
	if(!'C' %in% names(x))x$C <- rep(FALSE,nrow(x)) #syntax supports zero-row data.frame
	x$C <- as.comment(x$C)
	#Comment cannot be NA
	x$C[is.na(x$C)] <- FALSE
	
	#Every active source record should define exactly one of HOUR or DATETIME.
	active <- x[!x$C,]
	subj <- active$SUBJ
	hour     <- rep(NA,nrow(active))
	datetime <- rep(NA,nrow(active))
	if('HOUR'     %in% names(x))hour     <- active$HOUR
	if('DATETIME' %in% names(x))datetime <- active$DATETIME
	hour <- is.na(hour)
	datetime <- is.na(datetime)
	exclusive <- xor(hour,datetime)
	if(any(!exclusive))stop(paste('exactly one of HOUR or DATETIME must be specified, e.g. SUBJ',subj[!exclusive][[1]]))

	#If DATETIME is present, definition (or not) should be constant within subject (for active records).
	if(!constant(datetime,within=subj))stop(paste('Both HOUR and DATETIME defined for SUBJ',subj[crosses(datetime,subj)][[1]]))
	#Coerce even in commented records
	#HOUR is received as-is, taken to represent relative accumulation of hours from arbitrary origin.
	x$TIME <- rep(NA,nrow(x))  #syntax supports zero-row data.frame
	if('HOUR' %in% names(x)) x$TIME <- x$HOUR
	#DATETIME is understood as seconds, coercible to miDateTime.
	if('DATETIME' %in% names(x))x$DATETIME <- as.miDateTime(x$DATETIME)	
	if('DATETIME' %in% names(x))x$TIME[!is.na(x$DATETIME)] <- as.numeric(x$DATETIME[!is.na(x$DATETIME)])/60/60
	
	#At this point, active TIME should be completely defined.
	if(any(is.na(x$TIME[!x$C])))stop('TIME not completely defined.')
	
	#SEQ will be imputed if not present
	if(!'SEQ' %in% names(x))x$SEQ <- rep(0,nrow(x))
	x$SEQ <- as.flag(x$SEQ)
	
	#x may be a data.frame, but now has enough information to define the nm key.
	x <- as.keyed(x,key=c('SUBJ','TIME','SEQ'))
	
	#Time is either relative hours from arbitrary origins, or hours since DATETIME origin.
	#Relativize to earliest value.
	
	x <- sort(x) #NAs will be last
	x$TIME <- signif(x$TIME - first(x$TIME,where=!is.na(x$TIME),within=x$ID),7)
	
	#PRIME
	#If data set contains AMT, prime can be calculated as the first non(commented) dose at any
	#given time within Subject.
	prime <- logical(0)
	if('AMT' %in% names(x))prime <- at(!is.na(x$AMT) & !x$C,within=list(x$ID,x$TIME))
	prime[is.na(prime)] <- FALSE
	
	#TAFD
	#Time After First Dose. The time of the first defined amount per subject is a local origin.
	#Domain is active records.  Range is all records.
	if(length(prime))x$TAFD <- x$TAFD <- signif(x$TIME - first(x$TIME,where=prime,within=x$ID),6)
	
	#TAD
	#Time After Dose.  
	#Each prime is a local origin.
	#Domain is active prime records.  Range is all records.
	#Where ADDL and II are present, TAD is calculated as well.
	tMostRecentDose <- function(x,y,a,i){
		#returns max value of y + (0:a)*i that is < x
		z <- y
		hi <- max(a,na.rm=TRUE) 
		if(is.na(hi) | is.nan(hi) | is.infinite(hi)) return(z)
		for(trial in 0:hi){
			candidate <- y + trial*i
			updatable <- !is.na(candidate) & !is.na(x) & !is.na(a) & candidate < x & trial <= a  
			z[updatable] <- candidate[updatable]
		}
		z							
	}
	s <- suppressWarnings
	if(length(prime))x$TAD <- signif(x$TIME - s(first(x$TIME,where=prime & !x$C,within=list(x$ID,cumsum(prime)))),5)
	if(length(prime) & all(c('ADDL','II') %in% names(x)))x$TAD <- signif(
		x$TIME - tMostRecentDose(
			x$TIME,#ceiling reference
			s(first(x$TIME, where=prime            ,    within=list(x$ID,cumsum(prime)))), # most recent dose record
			s(first(x$ADDL, where=!is.na(x$ADDL) & !x$C,within=list(x$ID,cumsum(prime)))), # most recent ADDL value
			s(first(x$II,   where=!is.na(x$II)   & !x$C,within=list(x$ID,cumsum(prime))))  # most recent II value
		),
		5
	)	
	#Impute flags.  Check whether merge drops flag status.
	flags <- names(x)[sapply(names(x),function(col)inherits(x[[col]],'flag'))]
	#x <- as.keyed(cbind(x[,names(x)[!names(x) %in% flags],drop=FALSE],data.frame(lapply(x[,flags,drop=FALSE],function(col){col[is.na(col)] <- 0;return(col)})))[names(x)],key=key(x))
	for(f in flags)x[[f]][is.na(x[[f]])] <- 0
		
	#LDOS
	#AMT from prime records is carried forward.
	#Domain is active prime records.  Range is all records.
	if(length(prime))x$LDOS <- s(first(x$AMT,where=prime & !x$C,within=list(x$ID,cumsum(prime))))
	
	#MDV
	if('DV' %in% names(x)){
		if(!'MDV' %in% names(x))x$MDV <- as.numeric(NA)
		x$MDV[is.na(x$MDV)] <- as.numeric(is.na(x$DV[is.na(x$MDV)]))
	}
	
	#Order
	#nonkey <- setdiff(setdiff(names(x),key(x)),'C')
	#x <- x[c('C',key(x),nonkey)]
	x <- shuffle(x,'C')
	x <- shuffle(x,key(x),after='C')
	row.names(x) <- NULL
	if(!inherits(x,'nm'))class(x) <- c('nm',class(x))
	x
}


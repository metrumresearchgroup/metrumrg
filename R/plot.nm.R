plot.nm <- function(
	x,
	which=NULL,
	dvname='DV',
	ivname='AMT',
	covariates=NULL,
	categorical=NULL,
	continuous=NULL,
	by=NULL,
	...
){
	if(is.null(covariates))covariates <- setdiff(
		names(x),
		c(
			'ID','SUBJ','TIME','DATETIME','TAFD',
			'TAD','ADDL','II','C','SEQ','LDOS','MDV',ivname,dvname
		)
	)
	if(is.null(continuous))continuous <- covariates[
		sapply(
			x[,covariates],
			function(x)is.numeric(x) & length(unique(x)) >= 7
		)
	]
	else continuous <- intersect(covariates,continuous)
	if(is.null(categorical))categorical <- setdiff(covariates,continuous)
	else categorical <- intersect(covariates,categorical)
	if(is.null(which))which <- names(nmPlots)
	if(is.null(by))by <- character(0)
	mins <- cast(
		melt(
			x,
			id.var='ID',
			measure.var='TIME'
		),
		fun.aggregate=function(x)min(x,na.rm=TRUE)
	)
	names(mins) <- c('ID','min')
	x <- stableMerge(x,mins)
	x$TIME <- with(x,TIME-min)
	x$min <- NULL
	x$DV <- x[[dvname]]
	lapply(
		nmPlots[which],
		function(p)p(
			data=x,
			dvname=dvname,
			ivname=ivname,
			categorical=categorical,
			continuous=continuous,
			by=by,
			...
		)
	)
}
nmPlots <- list(
	kinetics = function(
		formula=value~TIME|factor(SUBJ),
		data,
		dvname,
		ivname,
		by,
		id.var=c('C','ID','TIME','SUBJ','SEQ'),
		layout=c(3,5),
		as.table=TRUE,
		model='IPRE',
		scales=list(x=list(relation='free')),
		...
	){
	    formula <- as.formula(format(formula))
	    if (!'ADDL' %in% names(data))data$ADDL <- NA
	    if (!'II' %in% names(data))data$II <- NA
	    if (!'SS' %in% names(data))data$SS <- NA
	    if (!model %in% names(data))data[[model]] <- NA
	    if (!'EVID' %in% names(data))data$EVID <- ifelse(is.na(data[[ivname]]), 0, 1)
	    molten <- melt(data, id.var = union(id.var, c('SS', 'ADDL','II', 'EVID')), measure.var = c(ivname, dvname, model))
	    if ("EVID" %in% names(data))molten <- molten[with(molten, !(is.na(value) & EVID !=1 & variable == ivname)), ]
	    if ('EVID' %in% names(data))molten <- molten[with(molten, !(is.na(value) & EVID !=0 & variable == dvname)), ]
	    molten <- molten[with(molten, !(is.na(value) & variable ==model)), ]
	    dvscale <- max(c(molten$value[molten$variable %in% c(dvname,model)],1), na.rm = TRUE)
	    ivscale <- max(molten$value[molten$variable == ivname], na.rm = TRUE)
	    iv <- molten$variable == ivname
	    molten$value[iv] <- molten$value[iv]/ivscale * dvscale
	    molten$value[is.na(molten$value)] <- -(0.05 * dvscale)
	    #if (!all(is.na(molten$ADDL) == is.na(molten$II))) stop("addl ii mismatch")
	    molten$addl <- FALSE
	    if (any(!is.na(molten$ADDL))) {
		gratis <- molten[molten$variable == ivname & !is.na(molten$ADDL),]
		gratis <- do.call(
			rbind,
			by(
				gratis,
				rownames(gratis), 
		    function(x) {
			start <- x$TIME
			times <- x$ADDL
			inter <- x$II
			x <- x[rep(1, times), ]
			x$TIME <- x$TIME + cumsum(rep(inter, times))
			x
		    }
		  )
		)
		gratis$addl <- TRUE
		molten <- rbind(molten, gratis)
	    }
	    groups <- rep('sample', nrow(molten))
	    groups[molten$addl == TRUE] <- "addl"
	    groups[molten$EVID %in% c(1,4) & molten$addl == FALSE] <- 'dose'
	    groups[molten$variable == model] <- 'model'
	    groups[molten$C == TRUE] <- paste(groups[molten$C == TRUE],'comment')
	    ss <- !is.na(molten$SS) & molten$SS==1
	    groups[ss] <- paste(groups[ss],"steady")
	    groups <- factor(groups)
	    groupKey <- levels(groups)
	    xyplot(
	    	x = formula,
	    	data = molten,
	    	layout = layout,
	    	as.table = as.table, 
			scales = scales,
			groups = groups,
			panel = function(x,y, ...) {
		    panel.abline(h = 0, col = 'grey')
		    panel.superpose(x, y, ...)
		},
		panel.groups = function(x, y, group.number, ...) {
		    key <- groupKey[group.number]
		    col='blue'
		    if (contains('addl',key))col = 'lightblue'
		    if (contains('comment', key))col = 'magenta'
		    if (contains('model', key))col = 'green'
		    if (contains('dose', key))panel.segments(x, y, x, 0, col = col)
		    if (contains('addl', key))panel.segments(x, y, x, 0, col = col)
		    xlimits <- current.panel.limits()$x
		    width <- xlimits[[2]] - xlimits[[1]]
		    tick <- width / 100
		    if (contains('steady',key))panel.segments(x, y, x-tick, y, col = col)
		    if (contains('sample', key))panel.xyplot(x, y, col = col)
		    if (contains('model', key))panel.lines(x[order(x)], y[order(x)], col = col)
		},
		...
		)
	},
	constantContinuous = function(
		data,
		continuous,
		by,
		layout=3:4,
		as.table=TRUE,
		scales=list(y=list(relation='free')),
		...
	){
		if(!length(continuous))return()
		continuous <- continuous[
			sapply(
				continuous,
				function(x)constant(
					data[[x]],
					within=data$ID
				)
			)
		]
		if(!length(continuous))return()
		allC <- sapply(split(data$C,data$ID),all)
		data <- stableMerge(
			data,
			data.frame(ID=as.numeric(names(allC)),allC=allC)
		)
		data$C <- data$allC		
		data=data[!duplicated(data$ID),]
		molten <- melt(data,id.var=c('C','ID'),measure.var=continuous)
		xyplot(
			value ~ ID | variable,
			data=molten,
			groups=ifelse(C,'commented','active'),
			layout=layout,
			as.table=as.table,
			scales=scales,
			auto.key=TRUE,
			...
		)
	},
	varyingContinuous = function(
		data,
		continuous,
		by,
		layout=c(1,1),
		as.table=TRUE,
		scales=list(relation='free'),
		sets=1,
		type='l',
		...
	){
		if(!length(continuous))return()
		continuous <- continuous[
			sapply(
				continuous,
				function(x)!constant(
					data[[x]],
					within=data$ID
				)
			)
		]
		if(!length(continuous))return()
		data$set <- 'all'
		if(sets > 1) data$set <- cut(
			data$ID,
			breaks=round(
				quantile(
					data$ID,
					probs = seq(
						from=0,
						to=1,
						length.out=sets+1
					)
				)
			),
			include.lowest=TRUE
		)
		molten <- melt(
			data,
			id.var=c('C','ID','TIME','set'),
			measure.var=continuous
		)
		xyplot(
			value ~ TIME | set + variable,
			data=molten,
			allow.multiple=TRUE,
			groups=ID,
			layout=layout,
			as.table=as.table,
			scales=scales,
			type=type,
			panel=function(auto.key,groups,subscripts,...){
				current <- current.viewport()
				current$gp <- gpar(alpha=0.5)
				pushViewport(current)
				panel.superpose(
					groups=groups,
					subscripts=subscripts,
					...
				)
				popViewport()
			},
			panel.groups=function(x,y,col,col.line,...){
				defined <- is.finite(y) & is.finite(x)
				panel.xyplot(
					x[defined],
					y[defined],
					col=col,
					col.line=col.line,
					...
				)
				panel.rug(
					x=x[!defined],
					y=y[!defined],
					col=col.line,
					...
				)
			},
			...
		)
	},
	constantCategorical = function(
		data,
		categorical,
		by,
		layout=3:4,
		as.table=TRUE,
		...
	){
		if(!length(categorical))return()
		categorical <- categorical[
			sapply(
				categorical,
				function(x)constant(
					data[[x]],
					within=data$ID
				)
			)
		]
		if(!length(categorical))return()
		#data=data[!data$C,c('ID',categorical)]
		allC <- sapply(split(data$C,data$ID),all)
		data <- stableMerge(
			data,
			data.frame(ID=as.numeric(names(allC)),allC=allC)
		)
		data$C <- data$allC		
		data=data[!duplicated(data$ID),]
		molten <- melt(data,id.var=c('C','ID'),measure.var=categorical)
		stripplot(
			factor(value) ~ ID | variable,
			data=molten,
			layout=layout,
			groups=ifelse(C,'commented','active'),
			auto.key=TRUE,
			as.table=as.table,
			scales=list(y=list(relation='free')),
			prepanel=function(x,y)prepanel.default.bwplot(x,factor(y),...),
			panel=function(x,y,...)panel.stripplot(x,factor(y),jitter.data=TRUE,...),
			...
		)
	},
	varyingCategorical = function(
		data,
		categorical,
		by,
		layout=c(1,1),
		as.table=TRUE,
		type='l',
		sets=1,
		...
	){
		if(!length(categorical))return()
		categorical <- categorical[
			sapply(
				categorical,
				function(x)!constant(
					data[[x]],
					within=data$ID
				)
			)
		]
		if(!length(categorical))return()
		data$set <- 'all'
		if(sets > 1) data$set <- cut(
			data$ID,
			breaks=round(
				quantile(
					data$ID,
					probs = seq(
						from=0,
						to=1,
						length.out=sets+1
					)
				)
			),
			include.lowest=TRUE
		)
		molten <- melt(data,id.var=c('C','ID','TIME','set'),measure.var=categorical)
		molten$value <- as.factor(molten$value)
		stripplot(
			as.character(value) ~ TIME | set + variable,
			data=molten,
			#groups=ifelse(molten$C,'commented','active'),
			groups=ID,
			#auto.key=auto.key,
			layout=layout,
			as.table=as.table,
			type=type,
			scales=list(relation='free'),
			#prepanel=function(x,y)prepanel.default.bwplot(x,factor(y)),
			panel=panel.superpose,
			panel.groups=function(x,y,col,col.line,...){
				#y <- factor(y)
				defined <- !is.na(y) & is.finite(x)
				panel.stripplot(x[defined],y[defined],col=col,col.line=col.line,...)
				panel.rug(x=x[!defined],y=y[!defined], col=col.line,...)
			},
			...
		)
	}
)

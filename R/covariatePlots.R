`covariatePlots` <-
function (
	data,
	cont.cov=NULL, 
	cat.cov=NULL,
	par.list=NULL, 
	eta.list=NULL, 
	...
) 
{
    plots <- list()
    stopifnot("ID" %in% names(data))
    if(length(cont.cov)) cont.cov <- intersect(cont.cov,names(data))
    if(length(cat.cov)) cat.cov <- intersect(cat.cov,names(data))
    if(length(par.list)) par.list <- intersect(par.list,names(data))
    if(length(eta.list)) eta.list <- intersect(eta.list,names(data))
    for(cov in cat.cov)data[[cov]] <- factor(data[[cov]])
    data <- data[!duplicated(data$ID),]
    #Covariate SPLOM
    if (length(cont.cov) >= 2)plots$covSplom <- splom(
    	data[, cont.cov], 
    	panel = function(x, y) {
        	panel.splom(x, y)
            panel.lines(lowess(x,y))
        },
        main="Covariate Scatterplots",
        xlab="",
        pscales=0,
	...
    )
    #Cont vs cat bwpots
    if (length(cont.cov) & length(cat.cov)) {
        molten <- melt(data,measure.var=cont.cov, id.var=cat.cov)
        names(molten)[names(molten)=="variable"] <- "cont"
    	names(molten)[names(molten)=="value"] <- "y"
    	plasma <- melt(molten,measure.var=cat.cov)
    	names(plasma)[names(plasma)=="variable"] <- "cat"
    	names(plasma)[names(plasma)=="value"] <- "x"
    	plots$contCat <- bwplot(
    		y ~ factor(x) | cont + cat,
    		plasma,
    		as.table=TRUE,
    		layout=c(2,2),
    		horizontal=FALSE,
    		ylab="continuous covariate",
    		xlab="categorical covariate",
    		scales=list(relation="free"),
    		prepanel=function(x,y,...)prepanel.default.bwplot(factor(x),y,...),
    		panel=function(x,y,...)panel.bwplot(factor(x),y,...),
    		main="Continuous Covariates vs. Categorical Covariates",
		...
    	)
    }
    #ETA SPLOM
	if (length(eta.list) >= 2) {
        plots$etaSplom <- splom(
        	data[, eta.list], 
        	panel = function(x, y) {
            	panel.splom(x, y)
            	panel.lines(lowess(x,y))
        	},
        	main="ETA Scatterplots",
        	xlab="",
        	pscales=0,
		...
        )
    }
    #Parmater SPLOM
    if (length(par.list) >= 2) {
        plots$paramSplom <- splom(
        	data[, par.list], 
        	panel = function(x, y) {
            	panel.splom(x, y)
            	panel.lines(lowess(x,y))
        	},
        	main="Parameter Scatterplots",
        	xlab="",
        	pscales=0,
		...
        )
    }
    #ETA Histograms
    if(length(eta.list)){
    	etas <- melt(data,measure.var=eta.list)
    	plots$etaHist <- histogram(
    		~ value | variable,
    		etas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Histograms of Etas",
    		breaks=NULL,
    		scales=list(relation="free"),
		...
    	)
    }
    #ETA Densityplots
    if(length(eta.list)){
    	etas <- melt(data,measure.var=eta.list)
    	plots$etaDens <- densityplot(
    		~ value | variable,
    		etas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Density of Etas",
    		scales=list(relation="free"),
		...
    	)
    }
    #ETA vs Categoricals
    if(length(cat.cov) && length(eta.list)){
    	etas <- melt(data,measure.var=eta.list,id.var=cat.cov)
    	names(etas)[names(etas)=="variable"] <- "eta"
    	names(etas)[names(etas)=="value"] <- "delta"
    	condEtas <- melt(id.var=c("eta","delta"),etas)
    	plots$etaCat <- bwplot(
    		delta ~ factor(value) | variable + eta,
    		condEtas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Boxplots of Etas by Categorical Covariate",
    		horizontal=FALSE,
    		scales=list(relation="free"),
    		prepanel=function(x,y,...)prepanel.default.bwplot(factor(x),y,...),
    		panel=function(x,y,...)panel.bwplot(factor(x),y,...),
    		ylab="ETA",
    		xlab="categorical covariate level",
		...
    	)
    }
    #ETAS vs. Continuous
    if (length(cont.cov) && length(eta.list)) {
    	etas <- melt(data,measure.var=eta.list,id.var=cont.cov)
    	names(etas)[names(etas)=="variable"] <- "eta"
    	names(etas)[names(etas)=="value"] <- "delta"
    	condEtas <- melt(id.var=c("eta","delta"),etas)
    	plots$etaCont <- xyplot(
    		delta ~ value | variable + eta,
    		condEtas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Etas vs. Continuous Covariates",
    		ylab="ETA",
    		xlab="continuous covariate",
    		scales=list(relation="free"),
    		panel=function(x,y,...){
    			panel.xyplot(x,y,...)
    			panel.abline(h=0)
    			panel.lines(lowess(x,y),lty=2,col="red",...)
    		},
		...
    	)
    }
    plots
}
`cwresPlots` <-
function (
	data,
	cont.cov=NULL, 
	cat.cov=NULL,
	variant=NULL,
	...
) 
{
    res <- .resvar()
    if(is.null(variant)) variant <- res
    variant <- intersect(variant, names(data))
    plots <- list()
    if(!length(variant))return(plots)
    if(length(variant)>1)return( # recursive reduction to a single variant
    	c(
    		cwresPlots(
    	    		data=data,
    	    		cont.cov=cont.cov,
    	    		cat.cov=cat.cov,
    	    		variant=variant[-length(variant)],
    	    		...
    	    	),
    	    	cwresPlots(
    	    		data=data,
    	    		cont.cov=cont.cov,
    	    		cat.cov=cat.cov,
    	    		variant=variant[length(variant)],
    	    		...
    	    	)
    	)
    )
    #If we got here, we have only one variant.
    if(length(cont.cov)) cont.cov <- intersect(cont.cov,names(data))
    if(length(cat.cov)) cat.cov <- intersect(cat.cov,names(data))
    for(cov in cat.cov)data[[cov]] <- factor(data[[cov]])
    names(data)[names(data)==variant] <- '.res' # canonical name for easy melt formulas
    #variant vs. Categoricals
    if(length(cat.cov)){
    	res <- melt(data,id.var='.res',measure.var=cat.cov)
    	plots[[glue(variant,'Cat')]] <- bwplot(
    		.res ~ factor(value) | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main=paste(variant,"vs. Categorical Covariates"),
    		xlab="categorical Covariate",
    		ylab=variant,
    		scales=list(relation="free"),
    		prepanel=function(x,y,...)prepanel.default.bwplot(factor(x),y,...),
    		panel=function(x,y,...)panel.bwplot(factor(x),y,...),
		...
    	)
    }
    #variant vs. Continuous
    if(length(cont.cov)){
    	res <- melt(data,id.var='.res',measure.var=cont.cov)
    	plots[[glue(variant,'Cont')]] <- xyplot(
    		.res ~ value | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main=paste(variant,"vs. Continuous Covariates"),
    		xlab="continuous covariate",
    		ylab=variant,
    		scales=list(relation="free"),
    		panel=function(x,y,...){
    			panel.xyplot(x,y,...)
    			panel.abline(h=0)
    			panel.lines(lowess(x,y),lty=2,col="red",...)
    		},
		...
    	)
    }
    plots
}
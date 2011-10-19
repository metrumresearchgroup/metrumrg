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
	...
) 
{
    plots <- list()
    #CWRES
    #CWRES vs. Categoricals
    if("CWRES" %in% names(data) && length(cat.cov)){
    	res <- melt(data,id.var="CWRES",measure.var=cat.cov)
    	plots$cwresCat <- bwplot(
    		CWRES ~ factor(value) | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="CWRES vs. Categorical Covariates",
    		xlab="categorical Covariate",
    		ylab="conditional weighted residuals",
    		scales=list(relation="free"),
    		prepanel=function(x,y,...)prepanel.default.bwplot(factor(x),y,...),
    		panel=function(x,y,...)panel.bwplot(factor(x),y,...),
		...
    	)
    }
    #CWRES vs. Continuous
    if("CWRES" %in% names(data) && length(cont.cov)){
    	res <- melt(data,id.var="CWRES",measure.var=cont.cov)
    	plots$cwresCont <- xyplot(
    		CWRES ~ value | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="CWRES vs. Continuous Covariates",
    		xlab="continuous covariate",
    		ylab="conditional weighted residuals",
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
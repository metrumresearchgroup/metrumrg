`diagnosticPlots` <-
function (data, dvname='DV', group=NULL, model=NULL, include.all=FALSE,...) 
{
  plots <-list()
  value <- NULL # prevent warning from R CMD check
  variable <- NULL # prevent warning from R CMD check
  if(is.null(group))data$grpnames <- 'all'
  if(is.null(group))group <- 'grpnames'
  if(length(unique(data[[group]])) > 1 & include.all) plots <- diagnosticPlots(
  	data=data,
	dvname=dvname,
	group=NULL,
	model=model,
	include.all=FALSE,
	...
  )
  preds <- c('PRED','NPRED','CPRED','CPREDI','EPRED','IPRE','IPRED')
  obsvar <- intersect(preds,names(data))
  obsid <- c("DV","grpnames")
  observed <- list()
  if(length(obsvar) & length(obsid))observed <- melt(data,measure.var=obsvar,id.var=obsid)
  res <- c('RES','NRES','NWRES','CRES','CWRES','RESI','WRESI','CRESI','CWRESI','ERES','EWRES','ECWRES')
  resvar <- intersect(res,names(data))
  resid <- intersect(c("PRED","TIME","grpnames","TAD"),names(data))
  res <- list()
  if(length(resvar) & length(resid))res <- melt(data,measure.var=resvar,id.var=resid)
  groupSubtitle <- function(grp)paste("for",paste(grp,collapse=", "),"data")
  #Observed vs. Predicted 
  if(length(observed))if('DV' %in% names(observed)) for (group in unique(observed$grpnames))plots[[paste('obsPred',group)]] <- xyplot(
	DV ~ value |  variable, 
	observed[observed$grpnames==group,],
	as.table=TRUE,
	aspect=1,
	layout=c(2,2),
	ylab = paste("Observed", dvname), 
	xlab = paste("Predicted", dvname),
	panel= function(x,y,...){
		panel.xyplot(x,y,...)
		panel.abline(0,1)
	},
	main=paste(model,"\nObserved vs. Predicted",groupSubtitle(group)),
	...
   )
   h0loess <- function(x,y,lty=1,...){
  		panel.xyplot(x,y,lty=lty,...)
  		panel.abline(h=0,lty=lty,)
  		panel.loess(x,y,lty=2,...)
  }
   #Residuals vs. Predicted
   if(length(res))if('PRED' %in% names(res))for (group in unique(res$grpnames)) plots[[paste('resPred',group)]] <- xyplot(
  	value ~ PRED |  variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Predicted", dvname),
  	panel= h0loess,
  	scales=list(y=list(relation="free")),
  	main=paste(model,"\nResiduals vs. Predicted",groupSubtitle(group)),
	...
  )
  #Residuals vs. Time
  if(length(res)) if("TIME" %in% names(res))for (group in unique(res$grpnames))plots[[paste('resTime',group)]] <- xyplot(
  	value ~ TIME |  variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Time (hr)"),
  	panel= h0loess,
  	scales=list(y=list(relation="free")),
  	main=paste(model,"\nResiduals vs. Time",groupSubtitle(group)),
	...
  )
  #Residuals vs. TAD
  if(length(res))if("TAD" %in% names(res))for (group in unique(res$grpnames))plots[[paste('resTad',group)]] <- xyplot(
  	value ~ TAD |  variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Time (hr)"),
  	panel= h0loess,
  	scales=list(y=list(relation="free")),
  	main=paste(model,"\nResiduals vs. TAD",groupSubtitle(group)),
	...
  )
  #QQ-Norm
  if(length(res))for (group in unique(res$grpnames)) plots[[paste('resQ',group)]] <- qqmath(
  	~ value | variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	aspect=1,
  	ylab = "residuals", 
  	xlab = paste("theoretical quantiles"),
  	panel = function(x, ...) {
    	panel.qqmathline(x, ...)
    	panel.qqmath(x, ...)
    },
  	scales=list(y=list(relation="free")),
  	main=paste(model,"\nNormal Q-Q Plot Residuals",groupSubtitle(group)),
	qtype=7,
	...
  )
  plots
}


iterations <-
function(x,...){
	nodes <- grepl('ITERATION NO.:',x)
	if(sum(nodes)==0)stop('no iterations found')
	first <- first(where=nodes)
	second <- nth(where=nodes, n=2)
	period <- unique(second-first)
	iteration <- cumsum(nodes)
	dist <- distance(rep(TRUE,length(x)),within=iteration)
	inrange <- dist < period
	informative <- inrange & iteration > 0
	data <- x[informative]
	iteration <- iteration[informative]
	data <- split(data,iteration)
	
	iterationNo            <- function(x)text2decimal(strsplit(x[[1]],':')[[1]])[[2]]
	objectiveFunctionValue <- function(x)text2decimal(strsplit(x[[1]],':')[[1]])[[3]]
	noOfFuncEvals          <- function(x)text2decimal(strsplit(x[[1]],':')[[1]])[[4]]
	cumulativeNoOfFuncEvals<- function(x)text2decimal(gsub('\\.','',x[[2]]))
	gradientCue <- function(x)grep('GRADIENT',x)
	parameterCue <- function(x)grep('PARAMETER',x)
	paramlines <- function(x,pcue,gcue)x[pcue:(gcue-1)]
	gradientlines <- function(x,cue)x[cue:length(x)]
	params <- function(paramlines){
		paramlines <- sub('^ *','',paramlines)
		paramlines <- sub(' *$','',paramlines)
		paramlines <- sub('PARAMETER: +','',paramlines)
		res <- do.call(c,strsplit(paramlines,' +'))
		as.numeric(res)
	}
	grads <- function(gradientlines){
		gradientlines <- sub('^ *','', gradientlines)
		gradientlines <- sub(' *$','',gradientlines)
		gradientlines <- sub('GRADIENT: +','',gradientlines)
		res <- do.call(c,strsplit(gradientlines,' +'))
		as.numeric(res)
	}
	integrate <- function(i){
		pcue <- parameterCue(i)
		gcue <- gradientCue(i)
		pl <- params(paramlines(i,pcue,gcue))
		gl <- grads(gradientlines(i,gcue))
		pl <- data.frame(t(pl))
		gl <- data.frame(t(gl))
		pl$course <- 'parameter'
		gl$course <- 'gradient'
		x <- data.frame(
			iteration=iterationNo(i),
			ofv=objectiveFunctionValue(i),
			evals=noOfFuncEvals(i),
			cum=cumulativeNoOfFuncEvals(i)
		)
		rbind(
			cbind(x,pl),
			cbind(x,gl)
		)
	}
	res <- lapply(data,integrate)
	do.call(rbind,res)	
}


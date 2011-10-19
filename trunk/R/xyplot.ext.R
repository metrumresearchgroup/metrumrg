xyplot.ext <-
function(
	x,
	data=read.table(file,skip=1,header=TRUE,check.names=FALSE),
	project=getwd(),
	rundir=filename(project,x),
	file=filename(rundir,x,'.ext'),
	as.table=TRUE,
	auto.key=TRUE,
	layout=c(1,4),
	scales=list(relation='free'),
	type='l',
	panel=panel.superpose,
	panel.groups=function(x,y,group.number,type,...){
		if(group.number==3) type <- 'p'
		panel.xyplot(x=x,y=y,type=type,...)
	},
	...
){
	data <- data[data$ITERATION > -1000000002,]
	y <- melt(data,id='ITERATION')
	variable <- ''# to suppress R CMD check warning
	z <- cast(y,variable~ITERATION)
	z <- data.frame(z,check.names=FALSE)
	if(!'-1000000001'%in% names(z))z[,'-1000000001'] <- 0
	a <- melt(z,id=c('variable','-1000000000','-1000000001'))
	names(a) <- c('parameter','terminal','error','iteration','estimate')
	a$hi <- with(a, terminal + 1.96 * error)
	a$lo <- with(a, terminal - 1.96 * error)
	a$error <- NULL
	b <- melt(a, id=c('parameter','iteration'))
	b$iteration <- as.numeric(as.character(b$iteration))
	b$max <- with(b,reapply(iteration,INDEX=list(parameter,variable),FUN=max))
	b$iteration[b$variable!='estimate'] <- b$max[b$variable!='estimate']
	b$max <- NULL
	b <- unique(b)
	b$variable <- as.character(b$variable)
	b$variable[b$variable %in% c('hi','lo')] <- 'symmetric 95%CI'
	b$variable <- factor(b$variable)
	xyplot(
		value~iteration|parameter,
		data=b,
		groups=variable,
		as.table=as.table,
		auto.key=auto.key,
		layout=layout,
		scales=scales,
		type=type,
		panel=panel,
		panel.groups=panel.groups,
		...
	)
}


###################################################
### chunk number 1: model
###################################################
getwd()
library(MIfuns)
command <- '/common/NONMEM/nm7_osx1/test/nm7_osx1.pl'
cat.cov='SEX'
cont.cov=c('HEIGHT','WEIGHT','AGE')
par.list=c('CL','Q','KA','V','V2','V3')
eta.list=paste('ETA',1:10,sep='')


###################################################
### chunk number 2: run
###################################################
if(!file.exists('../nonmem/1005/diagnostics.pdf'))NONR(
     run=1005,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     checkrunno=FALSE,
     cont.cov=cont.cov,
     cat.cov=cat.cov,
     par.list=par.list,
     eta.list=eta.list,
     plotfile='../nonmem/*/diagnostics.pdf',
     streams='../nonmem/ctl'
)
getwd()
while(!file.exists('../nonmem/1005/diagnostics.pdf')){}


###################################################
### chunk number 3: predict
###################################################
ctl <- read.nmcontrol('../nonmem/ctl/1005.ctl')


###################################################
### chunk number 4: strip
###################################################
ctl[] <- lapply(ctl,function(rec)sub(' *;.*','',rec))
ctl


###################################################
### chunk number 5: fix
###################################################
ctl$prob
ctl$prob <- sub('1005','1105',ctl$prob)
names(ctl)
names(ctl)[names(ctl)=='theta'] <- 'msfi'
ctl$msfi <- '=../1005/1005/msf'
ctl$omega <- NULL
ctl$sigma <- NULL
names(ctl)[names(ctl)=='estimation'] <- 'simulation'
ctl$simulation <- 'ONLYSIM (1968) SUBPROBLEMS=500'
ctl$cov <- NULL
ctl$table <- NULL
ctl$table <- NULL
ctl$table <- 'DV NOHEADER NOPRINT FILE=./1105.tab FORWARD NOAPPEND'
write.nmcontrol('../nonmem/ctl/1105.ctl')


###################################################
### chunk number 6: sim
###################################################
if(!file.exists('../nonmem/1105/1105.lst'))NONR(
     run=1105,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     diag=FALSE,
     streams='../nonmem/ctl'
)
getwd()
while(!file.exists('../nonmem/1105/1105.lst')){}


###################################################
### chunk number 7: fetch
###################################################
phase1 <- read.csv('../data/derived/phase1.csv',na.strings='.')
head(phase1)
phase1 <- phase1[is.na(phase1$C),c('SUBJ','TIME','DV')]
records <- nrow(phase1)
records
phase1 <- phase1[rep(1:records,500),]
nrow(phase1)
phase1$SIM <- rep(1:500,each=records)
#head(phase1,300)
with(phase1,DV[SIM==1 & SUBJ==12])
with(phase1,DV[SIM==2 & SUBJ==12])


###################################################
### chunk number 8: preds
###################################################
pred <- scan('../nonmem/1105/1105.tab')
nrow(phase1)
length(pred)


###################################################
### chunk number 9: combine
###################################################
phase1$PRED <- pred
head(phase1)
phase1 <- phase1[!is.na(phase1$DV),]
head(phase1)


###################################################
### chunk number 10: subject
###################################################
head(phase1)
subject <- melt(phase1,measure.var=c('DV','PRED'))
head(subject)


###################################################
### chunk number 11: metrics
###################################################
metrics <- function(x)list(min=min(x), med=median(x), max=max(x))


###################################################
### chunk number 12: cast
###################################################
subject <- data.frame(cast(subject, SUBJ + SIM + variable ~ .,fun=metrics))
head(subject)


###################################################
### chunk number 13: metrics
###################################################
metr <- melt(subject,measure.var=c('min','med','max'),variable_name='metric')
head(metr)
metr$value <- reapply(
	metr$value,
	INDEX=metr[,c('SIM','variable','metric')],
	FUN=sort,
	na.last=FALSE
)
metr <- data.frame(cast(metr))
head(metr)
nrow(metr)
metr <- metr[!is.na(metr$DV),]#maybe no NA
nrow(metr)


###################################################
### chunk number 14: qq
###################################################
print(
	xyplot(
		PRED~DV|metric,
		metr,
		groups=SIM,
		scales=list(relation='free'),
		type='l',
		panel=function(...){
			panel.superpose(...)
			panel.abline(0,1,col='white',lwd=2)
		}
	)
)


###################################################
### chunk number 15: qqdetail
###################################################
med <- metr[metr$metric=='med',]
med$metric <- NULL
head(med)
trim <- inner(med, id.var=c('SIM'),measure.var=c('PRED','DV'))
head(trim)
nrow(trim)
trim <- trim[!is.na(trim$DV),]
nrow(trim)
head(trim)
print(
	xyplot(
		PRED~DV,
		trim,
		groups=SIM,
		type='l',
		panel=function(x,y,...){
			panel.xyplot(x=x,y=y,...)
			panel.abline(0,1,col='white',lwd=2)
			panel.abline(
				v=quantile(x,probs=c(0.25,0.5,0.75)),
				col='grey',
				lty=2
			)
		}
	)
)


###################################################
### chunk number 16: qqdensity
###################################################
head(trim)
quantile(trim$DV)
molt <- melt(trim, id.var='SIM')
head(molt)
quart <- data.frame(cast(molt,SIM+variable~.,fun=quantile,probs=c(0.25,0.5,0.75)))
head(quart)
molt <- melt(quart,id.var='variable',measure.var=c('X25.','X50.','X75.'),variable_name='quartile')
head(molt)
levels(molt$quartile)
levels(molt$quartile) <- c('first quartile','second quartile','third quartile')
head(molt)
levels(molt$variable)
molt$variable <- factor(molt$variable,levels=c('PRED','DV'))
print(
	densityplot(
		~value|quartile,
		molt,
		groups=variable,
		layout=c(3,1),
		scales=list(relation='free'),
		aspect=1,
		panel=panel.superpose,
		panel.groups=function(x,...,group.number){
			if(group.number==1)panel.densityplot(x,...)
			if(group.number==2)panel.abline(v=unique(x),...)
		},
		auto.key=TRUE
	)
)


###################################################
### chunk number 17: bootstrap
###################################################
getwd()
dir.create('../nonmem/1005.boot')
dir.create('../nonmem/1005.boot/data')
dir.create('../nonmem/1005.boot/ctl')


###################################################
### chunk number 18: control
###################################################
t <- metaSub(
     clear(readLines('../nonmem/ctl/1005.ctl'),';.+',fixed=FALSE),
     names=1:300,
     pattern=c(
         '1005',
         '../../data/derived/phase1.csv',
         '$COV',
         '$TABLE'
     ),
     replacement=c(
         '*',
         '../data/*.csv',
         ';$COV',
         ';$TABLE'
    ),
    fixed=TRUE,
    out='../nonmem/1005.boot/ctl',
    suffix='.ctl'
 )


###################################################
### chunk number 19: resample
###################################################
 bootset <- read.csv('../data/derived/phase1.csv')
 r <- resample(
 	bootset,
 	names=1:300,
 	key='ID',
 	rekey=TRUE,
 	out='../nonmem/1005.boot/data',
 	stratify='SEX'
 )


###################################################
### chunk number 20: boot
###################################################
if(!file.exists('../nonmem/1005.boot/CombRunLog.csv'))NONR(
     run=1:300,
     command=command,
     project='../nonmem/1005.boot/',
     boot=TRUE,
     nice=TRUE,
     streams='../nonmem/1005.boot/ctl'
)
getwd()  


###################################################
### chunk number 21: more
###################################################
#wait for bootstraps to finish
#while(!(all(file.exists(paste(sep='','../nonmem/1005.boot/',1:300,'.boot/',1:300,'.lst'))))){}
if(file.exists('../nonmem/1005.boot/log.csv')){
    boot <- read.csv('../nonmem/1005.boot/log.csv',as.is=TRUE)
}else{
    boot <- rlog(
	run=1:300,
	project='../nonmem/1005.boot',
	boot=TRUE,
	append=FALSE,
	tool='nm7'
    )
    write.csv(boot, '../nonmem/1005.boot/log.csv')
}
head(boot)
unique(boot$parameter)
text2decimal(unique(boot$parameter))
boot$X <- NULL


###################################################
### chunk number 22: pars
###################################################
boot <- boot[!is.na(text2decimal(boot$parameter)),]
head(boot)
unique(boot$moment)
unique(boot$value[boot$moment=='prse'])


###################################################
### chunk number 23: drop
###################################################
boot <- boot[boot$moment=='estimate',]
boot$moment <- NULL
unique(boot$tool)
boot$tool <- NULL
head(boot)
unique(boot$value[boot$parameter %in% c('OMEGA2.1','OMEGA3.1','OMEGA3.2')])
unique(boot$parameter[boot$value=='0'])


###################################################
### chunk number 24: off
###################################################
boot <- boot[!boot$value=='0',]
any(is.na(as.numeric(boot$value)))
boot$value <- as.numeric(boot$value)
head(boot)


###################################################
### chunk number 25: clip
###################################################
boot <- inner(
	boot, 
	preserve='run',
	id.var='parameter',
	measure.var='value'
)
head(boot)
any(is.na(boot$value))
boot <- boot[!is.na(boot$value),]


###################################################
### chunk number 26: ctl2xml
###################################################
stream <- readLines('../nonmem/ctl/1005.ctl')
tail(stream)
doc <- ctl2xml(stream)
doc
params <- unique(boot[,'parameter',drop=FALSE])
params$defs <- lookup(params$parameter,within=doc)
params$labels <- lookup(params$parameter,within=doc,as='label')
params
boot$parameter <- lookup(boot$parameter,within=doc,as='label')
head(boot)


###################################################
### chunk number 27: covs
###################################################
covariates <- read.csv('../data/derived/phase1.csv',na.strings='.')
head(covariates)
with(covariates,constant(WEIGHT,within=ID))
covariates <- unique(covariates[,c('ID','WEIGHT')])
head(covariates)
covariates$WT <- as.numeric(covariates$WEIGHT)
wt <- median(covariates$WT)
wt
range(covariates$WT)


###################################################
### chunk number 28: cuts
###################################################
head(boot) 
unique(boot$parameter)
clearance <- boot[boot$parameter %in% c('CL/F','WT.CL','Male.CL'),]
head(clearance)
frozen <- data.frame(cast(clearance,run~parameter),check.names=FALSE)
head(frozen)
frozen$WT.CL65 <- (65/70)**frozen$WT.CL
frozen$WT.CL75 <- (75/70)**frozen$WT.CL
frozen$WT.CL85 <- (85/70)**frozen$WT.CL


###################################################
### chunk number 29: key
###################################################
cl <- median(boot$value[boot$parameter=='CL/F'])
cl
head(frozen)
frozen[['CL/F']] <- frozen[['CL/F']]/cl
head(frozen)
frozen$WT.CL <- NULL
molten <- melt(frozen,id.var='run',na.rm=TRUE)
head(molten)


###################################################
### chunk number 30: covplot
###################################################
levels(molten$variable)
molten$variable <- factor(molten$variable,levels=rev(levels(molten$variable)))
print(stripplot(variable~value,molten,panel=panel.covplot))


###################################################
### chunk number 31: params
###################################################
library(Hmisc)
tab <- partab(1005,'../nonmem',tool='nm7',as=c('label','latex','model','estimate','unit','prse','se'))
tab$estimate <- as.character(signif(as.numeric(tab$estimate),3))
tab$estimate <- ifelse(is.na(tab$unit),tab$estimate,paste(tab$estimate, tab$unit))
tab$unit <- NULL
tab$label <- ifelse(is.na(tab$latex),tab$label,paste(tab$label, ' (',tab$latex,')',sep=''))
tab$latex <- NULL
names(tab)[names(tab)=='label'] <- 'parameter'
tab$root <- signif(sqrt(exp(as.numeric(tab$estimate))-1),3)
tab$estimate <- ifelse(contains('Omega|sigma',tab$parameter),paste(tab$estimate,' (\\%CV=',tab$root*100,')',sep=''),tab$estimate)
tab$root <- NULL
#offdiag <- contains('2.1',tab$parameter)
#tab$estimate[offdiag] <- text2decimal(tab$estimate[offdiag])
#omegablock <- text2decimal(tab$estimate[contains('Omega..(1|2)',tab$parameter)])
#cor <- signif(half(cov2cor(as.matrix(as.halfmatrix(omegablock))))[[2]],3)
#tab$estimate[offdiag] <- paste(sep='',tab$estimate[offdiag],' (COR=',cor,')')
tab$model[is.na(tab$model)] <- ''
#boot <- rlog(1:300,project='../nonmem/1005.boot',tool='nm7',boot=TRUE)
boot <- read.csv('../nonmem/1005.boot/log.csv',as.is=TRUE)
boot <- boot[boot$moment=='estimate',]
boot <- data.frame(cast(boot,...~moment))
boot[] <- lapply(boot,as.character)
boot <- boot[contains('THETA|OMEGA|SIGMA',boot$parameter),c('parameter','estimate')]
boot$estimate <- as.numeric(boot$estimate)
boot <- data.frame(cast(boot,parameter~.,value='estimate',fun=function(x)list(lo=as.character(signif(quantile(x,probs=0.05),3)),hi=as.character(signif(quantile(x,probs=0.95),3)))))
boot$CI <- with(boot, paste(sep='','(',lo,',',hi,')'))
names(boot)[names(boot)=='parameter'] <- 'name'
tab <- stableMerge(tab,boot[,c('name','CI')])
tab$name <- NULL
tab$se <- NULL


###################################################
### chunk number 32: 
###################################################
latex(
	tab,
	file='',
	rowname=NULL,
	caption='Parameter Estimates from Population Pharmacokinetic Model Run 1005',
	caption.lot='Model 1005 Parameters',
	label='p1005',
	where='ht',
	table.env=FALSE
)



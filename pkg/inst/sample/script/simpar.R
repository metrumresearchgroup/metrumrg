###################################################
### chunk number 1: data
###################################################
library(MIfuns)
data <- read.csv("../data/derived/phase1.csv")
head(data)


###################################################
### chunk number 2: cov
###################################################
cov <- read.table("../nonmem/1005/1005.cov", skip=1, header=T)
head(cov)


###################################################
### chunk number 3: rows
###################################################
cov<- cov[1:7,c(2:8)]


###################################################
### chunk number 4: sets
###################################################
set.seed(10)
PKparms <- simpar(
    nsim=10,
    theta=c(8.58,21.6, 0.0684, 3.78, 107, 0.999, 1.67),
    covar=cov,
    omega=list(0.196, 0.129, 0.107),
    odf=c(40,40,40),
    sigma=list(0.0671),
    sdf=c(200)
)
PKparms


###################################################
### chunk number 5: ctl
###################################################
ctl <- as.nmcontrol(readLines("../nonmem/ctl/1005.ctl"))
ctl[] <- lapply(ctl,function(rec)sub("<.*","",rec))


###################################################
### chunk number 6: set
###################################################
dir.create('../nonmem/sim')
set <- lapply(
	rownames(PKparms),
	function(row,params,ctl){
		params <- as.character(PKparms[row,])
		ctl$prob <- sub(1005,row,ctl$prob)
		ctl$theta <- params[1:7]
		ctl$omega <- params[8:10]
		ctl$sigma <- params[11]
		names(ctl)[names(ctl)=='estimation'] <- 'simulation'
		ctl$simulation <- paste(
			'(',
			as.numeric(row) + 7995,
			'NEW) (',
			as.numeric(row) + 8996,
			'UNIFORM) ONLYSIMULATION'
		)
		ctl$cov <- NULL
		ctl$table <- NULL
		ctl$table <- NULL
		ctl$table <- 'ID TIME DV WT SEX LDOS NOPRINT NOAPPEND FILE=sim.tab'
		write.nmcontrol(ctl,file=file.path('../nonmem/sim',paste(sep='.',row,'ctl')))
		return(ctl)		
	},
	params=PKparms,
	ctl=ctl
)


###################################################
### chunk number 7: nonr eval=FALSE
###################################################
## NONR(
## 	run=1:10,
## 	command="/common/NONMEM/nm7_osxi/test/nm7_osxi.pl",
## 	project="../nonmem/sim",
## 	diag=FALSE,
## 	checkrunno=FALSE,
## 	grid=TRUE
## )



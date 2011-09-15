###################################################
### chunk number 1: dir
###################################################
getwd()


###################################################
### chunk number 2: lib
###################################################
library(MIfuns)


###################################################
### chunk number 3: dose
###################################################
dose <- read.csv('../data/source/dose.csv',na.strings='.',stringsAsFactors=FALSE)
head(dose)
dose <- as.keyed(dose, key=c('SUBJ','HOUR'))
summary(dose)


###################################################
### chunk number 4: dem
###################################################
dem  <- read.csv('../data/source/dem.csv',na.strings='.',stringsAsFactors=FALSE)
head(dem)
dem <- as.keyed(dem, key='SUBJ')
summary(dem)


###################################################
### chunk number 5: pk
###################################################
pk   <- read.csv('../data/source/pk.csv',na.strings='.',stringsAsFactors=FALSE)
head(pk)
pk <- as.keyed(pk, key=c('SUBJ','HOUR'))
head(pk)
summary(pk)
pk[naKeys(pk),]
pk[dupKeys(pk),]
bad <- pk[with(pk,is.na(HOUR) |is.na(DV)),]
bad
pk <- pk - bad
summary(pk)


###################################################
### chunk number 6: assemble
###################################################
dat <- 
	nm() + 
	aug(dose,SEQ=1,EVID=1) + 
	aug(pk,  SEQ=0,EVID=0) | 
	dem

summary(dat)


###################################################
### chunk number 7: hide
###################################################
dat <- hide(dat, where=predoseDv(dat), why='predose')
summary(dat)


###################################################
### chunk number 8: zerodv
###################################################
dat <- hide(dat, where=zeroDv(dat), why='zerodv')
summary(dat)
head(dat)


###################################################
### chunk number 9: spec
###################################################
dat <- shuffle(dat,c('C','ID','TIME','SEQ','EVID','AMT','DV'))
head(dat)


###################################################
### chunk number 10: write
###################################################
write.nm(dat,file='../data/derived/phase1.csv')


###################################################
### chunk number 11: hidden
###################################################
summary(hidden(dat))



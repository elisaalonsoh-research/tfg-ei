### Ariel White
### spring 2016
### replication code: main analyses for "When Threat Mobilizes"

##PULL IN DATA AND SET UP
rm(list=ls())
setwd("/home/ariel/Desktop/SCanalysis/replicationdata")
library(foreign)
longdata <- read.csv("jurisdictiondata_incwhites.csv", sep = "\t", stringsAsFactors=F)
longdatafull<- longdata #save before dropping stuff

mean(na.omit(longdata$latinoturnout10)); max(na.omit(longdata$latinoturnout10)) 
longdata$whiteturnout10 <- longdata$whitevoters10 / longdata$whiteVAP2010
longdata$whiteturnout06 <- longdata$whitevoters06 / longdata$whiteVAP2006

#drop nonsense:
dim(longdata)
sum(is.na(longdata$change)) 
longdata[is.infinite(longdata$whiteturnout10)==T,"whiteturnout10"]<- NA
longdata[is.infinite(longdata$whiteturnout06)==T,"whiteturnout06"]<- NA
longdata[is.infinite(longdata$blackturnout10)==T,"blackturnout10"]<- NA
longdata[is.infinite(longdata$blackturnout06)==T,"blackturnout06"]<- NA
longdata[is.infinite(longdata$latinoturnout10)==T,"latinoturnout10"]<- NA
longdata[is.infinite(longdata$latinoturnout06)==T,"latinoturnout06"]<- NA
for (i in 1:nrow(longdata)){ 
	problem10 <- ifelse(longdata$latinoturnout10[i]>1, 1, 0)
	longdata$latinoturnout10[i] <- ifelse(problem10==1, NA, longdata$latinoturnout10[i])
	longdata$latinovote10[i] <- ifelse(problem10==1, NA,longdata$latinovote10[i])
	longdata$CVAP2010[i] <- ifelse(problem10==1, NA,longdata$CVAP2010[i])
	problem08 <- ifelse(longdata$latinoturnout08[i]>1, 1, 0)
	longdata$latinoturnout08[i] <- ifelse(problem08==1, NA, longdata$latinoturnout08[i])
	longdata$latinovote08[i] <- ifelse(problem08==1, NA,longdata$latinovote08[i])
	longdata$CVAP2008[i] <- ifelse(problem08==1, NA,longdata$CVAP2008[i])
	problem06 <- ifelse(longdata$latinoturnout06[i]>1, 1, 0)
	longdata$latinoturnout06[i] <- ifelse(problem06==1, NA, longdata$latinoturnout06[i])
	longdata$latinovote06[i] <- ifelse(problem06==1, NA,longdata$latinovote06[i])
	longdata$CVAP2006[i] <- ifelse(problem06==1, NA,longdata$CVAP2006[i])
}
#same for white turnout
for (i in 1:nrow(longdata)){
	problem10 <- ifelse(longdata$whiteturnout10[i]>1, 1, 0)
	longdata$whiteturnout10[i] <- ifelse(problem10==1, NA, longdata$whiteturnout10[i])
	longdata$whitevoters10[i] <- ifelse(problem10==1, NA,longdata$whitevoters10[i])
	longdata$CVAP2010[i] <- ifelse(problem10==1, NA,longdata$whiteVAP2010[i])
	problem06 <- ifelse(longdata$whiteturnout06[i]>1, 1, 0)
	longdata$whiteturnout06[i] <- ifelse(problem06==1, NA, longdata$whiteturnout06[i])
	longdata$whitevoters06[i] <- ifelse(problem06==1, NA,longdata$whitevoters06[i])
	longdata$CVAP2006[i] <- ifelse(problem06==1, NA,longdata$whiteVAP2006[i])
}

longdata$change <- longdata$latinoturnout10 - longdata$latinoturnout06
longdata$simpchange <- longdata$latinovote10 - longdata$latinovote06
longdata$whchange <- longdata$whiteturnout10 - longdata$whiteturnout06

#merge in state MOA dates.
moadates <- read.csv("stateMOAdates.csv", stringsAsFactors=F)
colnames(longdata); colnames(moadates)
abbrevs <- read.csv("statenames.csv", stringsAsFactors=F) #add full names to MOA data
moanames <- merge(moadates, abbrevs, by.x="State", "abbrev")
dim(moadates); dim(abbrevs); dim(moanames)
moadata <- merge(x=longdata, y=moanames, by.x="state", by.y="state", all.x=T)
dim(longdata); dim(moadates); dim(moadata)

moadata$sigdate <- as.Date(moadata$administratorsignature, format="%m/%d/%y")
moadata$impdate <- as.Date(moadata$activationdate, format="%m/%d/%Y")
moadata$byelectionday <- ifelse(moadata$impdate <= "2010-11-02"  , 1, 0)
moadata$dateofstats <- as.Date("08/31/12", format="%m/%d/%y")
moadata$timeenroll <- moadata$dateofstats - moadata$impdate
head(moadata$timeenroll)
moadata$imptoelection <- as.numeric(as.Date("2010-11-02") - moadata$impdate)
moadata$imptoelection <- ifelse(moadata$imptoelection<0, NA, moadata$imptoelection) ##no obs for places not enrolled by election day.
moadata$removalsadjusted <- (as.numeric(gsub(",", "", moadata$removals_total)) / as.numeric(moadata$timeenroll))*moadata$imptoelection
moadata$submissionsadjusted <- (as.numeric(gsub(",", "", moadata$submissions)) / as.numeric(moadata$timeenroll))*moadata$imptoelection
length(moadata$submissionsadjusted); sum(is.na(moadata$submissionsadjusted))
write.csv(moadata, file="MOAmergedjurdata_march2013.csv")

colnames(moadata)
colnames(moadata)[colnames(moadata)=="P0010001"] <- "totalpop"
colnames(moadata)[colnames(moadata)=="T002_002"] <- "popdens"
colnames(moadata)[colnames(moadata)=="T002_006"] <- "landarea"


##also, merge moadates with a list of states to have a list of all states and status.
library(foreign)
abbrevs <- read.csv("statenames.csv", stringsAsFactors=F)
moalist <- merge(x=moadates, y=abbrevs, by.x="State", by.y="abbrev", all.y=T)
dim(moadates); dim(abbrevs); dim(moalist)

moalist$admdate <- as.Date(moalist$administratorsignature, format="%m/%d/%y")
moalist$icedate <- as.Date(moalist$ICEsignature, format="%m/%d/%y")
moalist$byelectionday <- ifelse(moalist$admdate < "2010-11-02" , 2, 1)
moalist$byelectionday[is.na(moalist$byelectionday)] <- 1 

####################################################################################
#set up most conservative treatment var (last set of counties to enroll in state)
moadata$constreat <- 0

moadata[moadata$state=="Delaware","constreat"]<- 1
moadata[(moadata$state=="Florida" & moadata$impdate=="2010-06-22"),"constreat"]<- 1
moadata[(moadata$state=="Virginia" & moadata$impdate=="2010-06-15"),"constreat"]<- 1
moadata[(moadata$state=="West Virginia" & moadata$impdate=="2010-10-26"),"constreat"]<- 1
moadata[(moadata$state=="Texas" & moadata$impdate=="2010-09-28"),"constreat"]<- 1
sum(moadata$constreat)

moadatadrops1 <- moadatadrops <- subset(moadata, (moadata$constreat==1)|(moadata$constreat == 0 & moadata$impdate>="2010-11-02")|(moadata$constreat == 0 & is.na(moadata$impdate)==T)) #include treated (pre-election) and untreated (never enrolled or enrolled after election).
dim(moadatadrops); dim(moadata)


#make table for paper
library(xtable)
treatstates <- c("Delaware", "Florida", "Virginia", "Texas", "West Virginia")
treated <- subset(moadatadrops$state, (moadatadrops$state %in% treatstates))
untreated <- subset(moadatadrops$state, !(moadatadrops$state %in% treated))
treattab <-as.data.frame(table(treated))
untreattab <-as.data.frame(table(untreated)) 
colnames(untreattab) <- colnames(treattab) <- c("State", "Units")

samptab <- rbind(treattab, untreattab)

xtable(table(moadatadrops$state))
breakline <- round(dim(samptab)[1]/2)+ 1
colnames(samptab)<- c("State", "Units")
col1 <- samptab[1:breakline,]
col2 <- samptab[(breakline+1):(nrow(samptab)+1),]
new <- as.data.frame(cbind(col1, col2))
tab <- xtable(new)
print.xtable(tab,include.rownames=F)

##also make some descriptive tables.
colnames(moadatadrops)
treated <- subset(moadatadrops, moadatadrops$constreat==1)
untreated <- subset(moadatadrops, moadatadrops$constreat==0)
dim(treated); dim(untreated); dim(moadatadrops)
mean(treated$popdens); mean(untreated$popdens); mean(moadatadrops$popdens); mean(moadata$popdens)
mean(treated$CVAP2006); mean(untreated$CVAP2006, na.rm=T); mean(moadatadrops$CVAP2006, na.rm=T); mean(moadata$CVAP2006, na.rm=T)
latinopop <-cbind(mean(moadatadrops$CVAP2006, na.rm=T), mean(moadata$CVAP2006, na.rm=T))
totalpop <- cbind(mean(moadatadrops$T008_001, na.rm=T), mean(moadata$T008_001, na.rm=T))
popdens <- cbind(mean(moadatadrops$popdens, na.rm=T), mean(moadata$popdens, na.rm=T))

samplecompare <- rbind(latinopop, totalpop, popdens)
colnames(samplecompare) <- c("Sample","All jurisdictions")
rownames(samplecompare) <- c("Latino citizen population, 2006", "Total population, 2010", "Population density, 2010")

library(xtable)
xtable(samplecompare, digits=0)
head(moadata); names(moadatadrops)
a2 <- countiesin <- moadatadrops[,c(1:3, 198)]
a1 <- allcounties <- moadata[,c(1:3, 198)]

rows.in.a1.that.are.not.in.a2  <- function(a1,a2)
{
    a1.vec <- apply(a1, 1, paste, collapse = "")
    a2.vec <- apply(a2, 1, paste, collapse = "")
    a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
    return(a1.without.a2.rows)
}
dropped <- rows.in.a1.that.are.not.in.a2(a1,a2)
dim(dropped); dim(moadata); dim(moadatadrops)
head(dropped)

write.csv(dropped, "droppedcounties.csv")

head(longdatafull)
for (i in 1:nrow(longdatafull)){
	obs <- longdatafull$abbrev[i]
	longdatafull$abbrev[i]<- ifelse(obs=="", longdatafull$abbrev[i-1], obs)
}

droppedincleaning <- rows.in.a1.that.are.not.in.a2(a1,a2)
dim(droppedincleaning); dim(longdata); dim(longdatafull)
head(droppedincleaning)

write.csv(droppedincleaning, "droppedcounties_fromcleanup.csv")

##also just output the treated status of counties (for mapping later)
names(moadatadrops)
head(moadatadrops)
treatmenttab <- moadata[,c("state", "abbrev", "county", "State", "stateid", "GEOid2", "GEOid", "constreat")]
head(treatmenttab)
write.csv(treatmenttab, file="treatmentstatusofcounties.csv")

###########################################################################################
# organizing roughly by order of appearance in paper.  
# First table: simplest OLS.
head(moadata)
moadata[is.na(moadata$byelectionday)==T, "byelectionday"]<- 0

w0 <- lm(change~byelectionday, data=moadata)
summary(w0)

w1 <- lm(change~byelectionday + had_senelection2006 + had_senelection2010+ had_govelection2006 + had_govelection2010, data=moadata)
summary(w1)
library(stargazer)
stargazer(w1, covariate.labels = c("Enrolled in SC by election 2010", "Senate election 2010", 
	"Senate election 2006", "Governor election 2006", "Governor election 2010"), 
	title= "Observational approach: Comparing all jurisdictions enrolled in SC to all unenrolled jurisdictions",
	omit.stat=c("LL","ser","f", "AIC"), label="obs1", dep.var.labels=c("Turnout change, 2006-2010")) 

w2 <- lm(latinoturnout10 ~ latinoturnout06 + byelectionday + had_senelection2006 + had_senelection2010+ had_govelection2006 + had_govelection2010, data=moadata)
summary(w2)

############################################################################################
## Next, run it at county-level (though this likely understates the std. errors)

#OLS with clustered, robust SEs (code originally from Vavreck + Green)
library(rms) #"Design" library is out of date
regress2<-ols(change ~ constreat, data=moadatadrops1,method="qr",x=T);
cluster_regress2<-robcov(regress2, moadatadrops1$state, method=c('efron'));
print(cluster_regress2)
print(regress2)
cluster_regress2$coef
library(stargazer)
stargazer(cluster_regress2)

regress3<-ols(change ~ constreat + had_senelection2006 + had_senelection2010 + had_govelection2006 + had_govelection2010, data=moadatadrops1,method="qr",x=T);
cluster_regress3<-robcov(regress3, moadatadrops1$state, method=c('efron'));
print(cluster_regress3)

stargazer(cluster_regress2, cluster_regress3, covariate.labels = c("Treatment (Involuntary SC enrollment)", "Senate election 2010", "Senate election 2006", "Governor election 2006", "Governor election 2006"),title= "Jurisdiction-level difference-in-differences, with robust clustered standard errors",
	omit.stat=c("LL","ser","f", "AIC", "chi2"), label="RCSE1", dep.var.labels=c("Turnout change, 2006-2010") )


#plot these?
RCSE2coeff <- cluster_regress3$coef[2]
RCSE2se <- sqrt(diag(cluster_regress3$var))[2]

RCSE1coeff <- cluster_regress2$coef[2]
RCSE1se <- sqrt(diag(cluster_regress2$var))[2]

############################################################################################
## Run with varying intercepts

#library(arm)
library(lme4)
DDsimpmlm <- glmer(change ~ constreat+(1+constreat | state), data=moadatadrops1)
summary(DDsimpmlm) 

DDmainmlm <- glmer(change ~ constreat+ had_senelection2006 + had_senelection2010 +had_govelection2006 +had_govelection2010+(1+constreat | state), data=moadatadrops1)
summary(DDmainmlm)
coef(DDmainmlm)

DDmainmlm1 <- glmer(change ~ constreat+ had_senelection2006 + had_senelection2010 +had_govelection2006 +had_govelection2010+(1| state), data=moadatadrops1)
summary(DDmainmlm1)
stargazer(DDmainmlm1)
coef(DDmainmlm1)

DDwhmlm <- glmer(whchange ~ constreat+ had_senelection2006 + had_senelection2010 +had_govelection2006 +had_govelection2010+(1+constreat | state), data=moadatadrops1)
summary(DDwhmlm)


stargazer(DDsimpmlm,DDmainmlm, covariate.labels = c("Treatment (Involuntary SC enrollment)", "Senate election 2006", "Senate election 2010", "Governor election 2006", "Governor election 2006"),title= "County difference-in-differences, with state-varying intercepts",
	omit.stat=c("LL","ser","f", "AIC", "chi2"), label="mlm1", dep.var.labels=c("Turnout change, 2006-2010") )
stargazer(cluster_regress2, cluster_regress3,DDmainmlm, DDmainmlm1, covariate.labels = c("Constant", "Treatment (Involuntary SC enrollment)", "Senate election 2006", "Senate election 2010", "Governor election 2006", "Governor election 2006"),title= "Jurisdiction-level difference-in-differences",
	omit.stat=c("LL","ser","f", "AIC", "chi2"), label="mlm1", dep.var.labels=c("Turnout change, 2006-2010") )
#can combine "constant" rows later. this package has changed since generating this table for paper, but the basic idea is here.
stargazer(cluster_regress2, cluster_regress3)
stargazer(cluster_regress2, cluster_regress3)

state1coeff <- fixef(DDsimpmlm)[2]
state2coeff <- fixef(DDmainmlm)[2]
state1se <- sqrt(vcov(DDsimpmlm)[2,2])
state2se <- sqrt(vcov(DDmainmlm)[2,2])

ests <- as.data.frame(cbind(c(RCSE1coeff, RCSE2coeff, state1coeff, state2coeff),c(RCSE1se, RCSE2se, state1se, state2se)), row.names=c("RCSE1","RCSE2","state1","state2"))
colnames(ests) <- c("coef", "se")
ests$left <- ests$coef - 1.96*(ests$se)
ests$right <- ests$coef + 1.96*(ests$se)
ests$ruler <- c(1,2,3,4)

ests1 <- as.data.frame(apply(ests,  c(1,2), function(x) x*100))

### now plot as percentage points
pdf(file="mainSCcoeffs_percent.pdf")
par(mgp = c(3, 1, 0))
plot(ests$ruler, ests1$coef, xaxt='n', xlab="", ylim= c(-1, 4.5), 
	ylab="Change in turnout rate, 2010-2006 (percentage points)", cex.lab=1.15,
	main="Estimated change in Latino voter turnout (2006-2010) \n due to Secure Communities implementation", type="p", pch=19)
segments(ests$ruler, ests1$left, ests$ruler, ests1$right, lwd=2)
abline(h=0, lty=2)

par(mgp = c(5, 1.5, 0))
axis(side = 1, at = ests$ruler, labels = c("Clustered SE's, \n no covariates", "Clustered SE's, \n covariates", "Hierarchical model, \n no covariates", "Hierarchical model, \n covariates"), tck = -0.01)
dev.off()

#what's avg turnout?
colnames(moadatadrops1)
mean(moadatadrops$latinoturnout06, na.rm=T)
dim(moadatadrops1); dim(moadata)
names(moadata)

##########################################################################################################
#okay, other robustness stuff reported:

#######################
##POPULATION

## Is there something going on with tiny places?  (Few Latinos)
mean(moadatadrops$CVAP2006, na.rm=T); median(moadatadrops$CVAP2006, na.rm=T)
bigpopdata <- subset(moadatadrops1, CVAP2006>207)
dim(bigpopdata); dim(moadatadrops1)
did3d <- lm(change~ constreat, data=bigpopdata)
summary(did3d)

did3d <- lm(change~ constreat, data=bigpopdata)
summary(did3d)

library(lme4) #estimates may be marginally different from the paper due to package changes
did3e <- glmer(change~ constreat + had_senelection2004 + had_senelection2008 + had_govelection2004  +(1+constreat|state), data=bigpopdata)
summary(did3e)

########################## limit to fewer states 

## What if, instead of arbitrarily restricting to the south, I restricted to states that had
## at least one jurisdiction enrolled as of election 2010 (such that I know they're not just
## complete holdouts/being ignored by ICE)?
nonenrolled <- c("AL", "AK", "CO", "DC", "IN", "KS", "MN", "NH", "NJ", "NY", "ND", "RI", "VT", "WA", "WI", "WY")
statenames <- unique(moadatadrops$abbrev)
enrollstates <- setdiff(statenames,nonenrolled)
nonenrolled; enrollstates

moadataenroll <- subset(moadatadrops1, moadatadrops1$abbrev %in% enrollstates)
dim(moadataenroll); dim(moadatadrops1)

#this set uses only states that had at least one jurisdiction enrolled in SC by election 2010 
# so treatment of these might be more plausible than other states.

did3d <- lm(change~ constreat, data=moadataenroll)
summary(did3d)

did3e <- glmer(change~ constreat + had_senelection2004 + had_senelection2008 + had_govelection2004  +(1+constreat|state), data=moadataenroll)
summary(did3e)



#################################################
## look at only MOA signers?

head(stateclust1); dim(stateclust1)
stateclust1$state
moadates #from above; beginning of file.
moadates$sigdate <- as.Date(moadates$administratorsignature, format="%m/%d/%y")
moadates$byelectionday <- ifelse(moadates$sigdate <= "2010-11-02"  , 1, 0)
moasigners <- moadates[moadates$byelectionday==1, "State"]

moadatasign <- subset(moadatadrops1, moadatadrops1$abbrev %in% moasigners)
dim(moadatasign); dim(moadatadrops1)


#okay now run analysis only on states that signed MOAs before 2010 election

did3d <- lm(change~ constreat, data=moadatasign)
summary(did3d)

did3e <- glmer(change~ constreat + had_senelection2004 + had_senelection2008 + had_govelection2004  +(1+constreat|state), data=moadatasign)
summary(did3e)


###########################################################################
## what if my "conservative" treatment var is too narrow?  
head(moadata)

moadata$bigtreat <- 0
treatedst <- c("Delaware", "Florida", "Virginia", "Texas", "West Virginia")
moadata[moadata$state %in% treatedst ,"bigtreat"] <- 1

DID1 <- lm(change ~ bigtreat, data=moadata) 
summary(DID1)

DID2 <- glmer(change ~ bigtreat+ had_senelection2004 + had_senelection2008 + had_govelection2004  +(1+constreat|state), data=moadata) 
summary(DID2)

#might want to do this more tidily: drop the places that signed up before the MOA.
moadates
bigtreatdata <- moadata
bigtreatdata$bigtreat <- 0
bigtreatdata[bigtreatdata$state=="Virginia" & bigtreatdata$impdate > "2009-10-13", "bigtreat"]<- 1
bigtreatdata[bigtreatdata$state=="Delaware", "bigtreat"]<- 1 
bigtreatdata[bigtreatdata$state=="Texas" & bigtreatdata$impdate > "2009-10-26", "bigtreat"]<- 1
bigtreatdata[bigtreatdata$state=="Florida" & bigtreatdata$impdate > "2010-06-07", "bigtreat"]<- 1
bigtreatdata[bigtreatdata$state=="West Virginia" & bigtreatdata$impdate > "2010-02-25", "bigtreat"]<- 1

bigtreatdata$bigtreat <- 0 ##change only VA?
bigtreatdata[bigtreatdata$state=="Virginia" & bigtreatdata$impdate > "2009-10-27", "bigtreat"]<- 1
bigtreatdata[bigtreatdata$state=="Delaware", "bigtreat"]<- 1 
bigtreatdata[bigtreatdata$state=="Texas" & bigtreatdata$impdate =="2010-09-28", "bigtreat"]<- 1
bigtreatdata[bigtreatdata$state=="Florida" & bigtreatdata$impdate == "2010-06-22", "bigtreat"]<- 1
bigtreatdata[bigtreatdata$state=="West Virginia", "bigtreat"]<- 1


colnames(bigtreatdata)
sum(bigtreatdata$bigtreat)
#drop untreated units in treated states (self-selected into SC)
treats <- c("Florida", "Delaware", "Virginia", "Texas", "West Virginia")
bt1 <- subset(bigtreatdata, !(bigtreatdata$state %in% treats & bigtreatdata$bigtreat==0))
dim(bigtreatdata); dim(bt1)
DID1 <- lm(change ~ bigtreat, data=moadata) 
summary(DID1)


DID2 <- glmer(change ~ bigtreat+ had_senelection2004 + had_senelection2008 + had_govelection2004  +(1+constreat|state), data=moadata) 
summary(DID2)
############################################################################################
## Collapse by state and run it that way (as a robustness ck in appendix.)

longdata <- read.csv("jurisdictiondata_incwhites.csv", sep = "\t", stringsAsFactors=F)
longdatafull<- longdata #save before dropping stuff

mean(na.omit(longdata$latinoturnout10)); max(na.omit(longdata$latinoturnout10)) 
longdata[is.infinite(longdata$latinoturnout10)==T,"latinoturnout10"]<- NA
longdata[is.infinite(longdata$latinoturnout06)==T,"latinoturnout06"]<- NA

longdata$change <- longdata$latinoturnout10 - longdata$latinoturnout06

moadates <- read.csv("stateMOAdates.csv", stringsAsFactors=F)
colnames(longdata); colnames(moadates)
abbrevs <- read.csv("statenames.csv", stringsAsFactors=F) #add full names to MOA data
moanames <- merge(moadates, abbrevs, by.x="State", "abbrev")
dim(moadates); dim(abbrevs); dim(moanames)
MOAdata <- merge(x=longdata, y=moanames, by.x="state", by.y="state", all.x=T)
dim(longdata); dim(moadates); dim(MOAdata) #good, merge worked.

MOAdata$sigdate <- as.Date(MOAdata$administratorsignature, format="%m/%d/%y")
MOAdata$impdate <- as.Date(MOAdata$activationdate, format="%m/%d/%Y")
MOAdata$byelectionday <- ifelse(MOAdata$impdate <= "2010-11-02"  , 1, 0)
MOAdata$imptoelection <- as.numeric(as.Date("2010-11-02") - MOAdata$impdate)
MOAdata$imptoelection <- ifelse(MOAdata$imptoelection<0, NA, MOAdata$imptoelection) 

library(foreign)
abbrevs <- read.csv("statenames.csv", stringsAsFactors=F)
moalist <- merge(x=moadates, y=abbrevs, by.x="State", by.y="abbrev", all.y=T)
dim(moadates); dim(abbrevs); dim(moalist)

moalist$admdate <- as.Date(moalist$administratorsignature, format="%m/%d/%y")
moalist$icedate <- as.Date(moalist$ICEsignature, format="%m/%d/%y")
moalist$byelectionday <- ifelse(moalist$admdate < "2010-11-02" , 2, 1)
moalist$byelectionday[is.na(moalist$byelectionday)] <- 1 

MOAdata$constreat <- 0

MOAdata[MOAdata$state=="Delaware","constreat"]<- 1
MOAdata[(MOAdata$state=="Florida" & MOAdata$impdate=="2010-06-22"),"constreat"]<- 1
MOAdata[(MOAdata$state=="Virginia" & MOAdata$impdate=="2010-06-15"),"constreat"]<- 1
MOAdata[(MOAdata$state=="West Virginia" & MOAdata$impdate=="2010-10-26"),"constreat"]<- 1
MOAdata[(MOAdata$state=="Texas" & MOAdata$impdate=="2010-09-28"),"constreat"]<- 1
sum(MOAdata$constreat)

MOAdata$latinovote02 <- MOAdata$Early02 + MOAdata$Mail02 + MOAdata$Polling02 + MOAdata$Absentee02 + MOAdata$Unknown02
MOAdata$latinovote04 <- MOAdata$Early04 + MOAdata$Mail04 + MOAdata$Polling04 + MOAdata$Absentee04 + MOAdata$Unknown04


MOAdatadrops1 <- MOAdatadrops <- subset(MOAdata, (MOAdata$constreat==1)|(MOAdata$constreat == 0 & MOAdata$impdate>="2010-11-02")|(MOAdata$constreat == 0 & is.na(MOAdata$impdate)==T)) #include treated (pre-election) and untreated (never enrolled or enrolled after election).
dim(MOAdatadrops); dim(MOAdata)

dim(MOAdatadrops); dim(MOAdatadrops1)
mean(MOAdatadrops1$CVAP2006)
for (i in 1:nrow(MOAdatadrops)){
MOAdatadrops$CVAP02alt[i] <-mean(c(MOAdatadrops$CVAP2002[i], MOAdatadrops$CVAP2006[i]))
MOAdatadrops$CVAP02alt1[i] <-((MOAdatadrops$CVAP2006[i] - MOAdatadrops$CVAP2002[i])/3)+MOAdatadrops$CVAP2002[i]
MOAdatadrops$CVAP04alt[i] <-((MOAdatadrops$CVAP2006[i] - MOAdatadrops$CVAP2002[i])*(2/3))+MOAdatadrops$CVAP2002[i]
}


library(plyr)
#sum up Latino pop in state and Latino votes in state for a given yr (dropping the self-selected)
#then divide to get turnout.  then generate change var.
stateclust <- ddply(MOAdatadrops, .(state), summarise, latpop06=sum(CVAP2006, na.rm=TRUE), latpop10=sum(CVAP2010, na.rm=TRUE),latpop06b=sum(latinoVAP2006b, na.rm=TRUE), latpop10b=sum(latinoVAP2010b, na.rm=TRUE), latpop06r=sum(latinoVAP2006_restrict, na.rm=TRUE), latpop10r=sum(latinoVAP2010_restrict, na.rm=TRUE), latpop02=sum(CVAP2002, na.rm=TRUE), latpop02alt=sum(CVAP02alt, na.rm=TRUE), latvote06 = sum(latinovote06, na.rm=TRUE), latvote10 = sum(latinovote10, na.rm=T), latvote02 = sum(latinovote02, na.rm=T), whitepop10 = sum(whiteVAP2010), whitepop06 = sum(whiteVAP2006), blackpop06 = sum(blackVAP2006), blackpop10 = sum(blackVAP2010), whitevote06 = sum(whitevoters06), whitevote10 = sum(whitevoters10), blackvote06 = sum(blackvoters06), blackvote10 = sum(blackvoters10))
stateclust$latinoturnout06 <- stateclust$latvote06/stateclust$latpop06
stateclust$latinoturnout10 <- stateclust$latvote10/stateclust$latpop10
stateclust$latinoturnout06b <- stateclust$latvote06/stateclust$latpop06b
stateclust$latinoturnout10b <- stateclust$latvote10/stateclust$latpop10b
stateclust$blackturnout06 <- stateclust$blackvote06/stateclust$blackpop06
stateclust$blackturnout10 <- stateclust$blackvote10/stateclust$blackpop10
stateclust$whiteturnout06 <- stateclust$whitevote06/stateclust$whitepop06
stateclust$whiteturnout10 <- stateclust$whitevote10/stateclust$whitepop10
dim(stateclust)
mean(stateclust$latinoturnout10)  #now this seems high, and the state-level #s are bad.

stateclust$statetreat <- 0
stateclust[stateclust$state %in% c("Delaware", "Florida", "Virginia", "Texas", "West Virginia"),"statetreat"]<- 1

stateclust$change0610 <- stateclust$latinoturnout10 - stateclust$latinoturnout06
stateclust$change0610b <- stateclust$latinoturnout10b - stateclust$latinoturnout06b
stateclust$change0610o <- stateclust$latvote10 - stateclust$latvote06
stateclust$change0610op <- (stateclust$latvote10 - stateclust$latvote06)/stateclust$latvote06
stateclust$whchange0610 <- stateclust$whiteturnout10 - stateclust$whiteturnout06
stateclust$blchange0610 <- stateclust$blackturnout10 - stateclust$blackturnout06


dd1 <- lm(change0610 ~ statetreat, data=stateclust)
summary(dd1) ##roughly the same estimate, but not significant.

covars <- read.csv("statecovariates.csv", stringsAsFactors=F) #merge in same electoral covars as above.
stateclust1 <- merge(stateclust, covars, by.x = "state", by.y = "state")
dim(stateclust); dim(covars); dim(stateclust1)
stateclust1$statetreat <- stateclust1$statetreat.x
colnames(stateclust1)

#weight by # of units in each state pre-collapse, as in Vavreck + Green
stateobs <- as.data.frame(table(moadatadrops1$state))
stateclustmain <- merge(stateclust1, stateobs, by.x="state", by.y="Var1")
dim(stateclustmain); colnames(stateclust1)

state2<- DD3e <- lm(change0610 ~ statetreat.x + had_senelection2010 + had_senelection2006+ had_govelection2006 + had_govelection2010, data=stateclustmain, weights = Freq)
summary(DD3e)

state1 <- DD3f <- lm(change0610 ~ statetreat.x, data=stateclustmain, weights = Freq)
summary(DD3f)

library(stargazer)
stargazer(DD3f, DD3e, covariate.labels = c("Treatment (Involuntary SC enrollment)", "Senate election 2010", 
	"Senate election 2006", "Governor election 2006", "Governor election 2010"), 
	title= "State-level difference-in-differences, weighted by # observations",
	omit.stat=c("LL","ser","f", "AIC", "chi2"), label="statetab2", dep.var.labels=c("Turnout change, 2006-2010")) #this is the state table in paper.

#check that weighting by latino CVAP doesn't totally kill the results:
DD3e_pop <- lm(change0610 ~ statetreat.x + had_senelection2010 + had_senelection2006+ had_govelection2006 + had_govelection2010, data=stateclustmain, weights = latpop06)
summary(DD3e_pop)

DD3f_pop <- lm(change0610 ~ statetreat.x, data=stateclustmain, weights = latpop06)
summary(DD3f_pop)

#substantively very close, esp with controls; not significant. 
write.csv(stateclustmain, "mainstateleveldata_sept2013.csv")

############################################################################################
## Other checks: Parallel Trends etc.

#does change 2002-2006 predict treatment?
#do this with states (where treatment was assigned)
stateclustmain$statepplchg0602 = (stateclustmain$latvote06 - stateclustmain$latvote02)/1000
stateclustmain$statepplchg0610 = (stateclustmain$latvote10 - stateclustmain$latvote06)/1000
stateclustmain$statechg0602pop = (stateclustmain$latinoturnout06 - (stateclustmain$latvote02/stateclustmain$latpop02))
stateclustmain$statechg0602popalt = (stateclustmain$latinoturnout06 - (stateclustmain$latvote02/stateclustmain$latpop02alt))
stateclustmain$statechg0610pop = (stateclustmain$latinoturnout10 - stateclustmain$latinoturnout06)


treat3 <- lm(statetreat ~ statepplchg0602, data=stateclustmain)
summary(treat3)

treat4 <- lm(statetreat ~ statepplchg0602, data=stateclustmain, weights=Freq)
summary(treat4)
treat4 <- lm(statetreat ~ statepplchg0602, data=stateclustmain, weights=latpop02)
summary(treat4)

treat3b <- lm(statetreat ~ statechg0602pop, data=stateclustmain)
summary(treat3b)
#Nothing here. 
#Also try looking at proportions.

stateclustmain$propchange0602 <- ifelse(stateclustmain$latvote02==0, 0, ((stateclustmain$latvote06 - stateclustmain$latvote02)/stateclustmain$latvote02))
mean(stateclustmain$propchange0602) #there are v. few for which turnout02 was 0, doesn't matter what I do with them.
treat3a <- lm(statetreat ~ propchange0602, data=stateclustmain)
summary(treat3a)
treat4a <- lm(statetreat ~ propchange0602, data=stateclustmain, weights=Freq)
summary(treat4a)

## also I want to do this for the full state as well, not just treated units in the state cluster.
moadata$state
moadata1 <- moadata
dim(moadata); dim(moadata1)
mean(moadata1$CVAP2006)

library(plyr)
#sum up Latino pop in state and Latino votes in state for a given yr (dropping the self-selected)
#then divide to get turnout.  then generate change var.
fullstateclust <- ddply(moadata1, .(state), summarise, latpop06=sum(CVAP2006, na.rm=T), latpop10=sum(CVAP2010, na.rm=T), latpop02=sum(CVAP2002, na.rm=T), latvote06 = sum(latinovote06, na.rm=T), latvote10 = sum(latinovote10, na.rm=T),  latvote02 = sum(latinovote02, na.rm=T))
fullstateclust$latinoturnout02 <- fullstateclust$latvote02/fullstateclust$latpop02
fullstateclust$latinoturnout06 <- fullstateclust$latvote06/fullstateclust$latpop06
fullstateclust$latinoturnout10 <- fullstateclust$latvote10/fullstateclust$latpop10
dim(fullstateclust)
mean(na.omit(fullstateclust$latinoturnout10))  

fullstateclust$statetreat <- 0
fullstateclust[fullstateclust$state %in% c("Delaware", "Florida", "Virginia", "Texas", "West Virginia"),"statetreat"]<- 1

fullstateclust$change0610 <- fullstateclust$latinoturnout10 - fullstateclust$latinoturnout06
fullstateclust$change0602 <- fullstateclust$latinoturnout06 - fullstateclust$latinoturnout02

covars <- read.csv("statecovariates.csv", stringsAsFactors=F)
fullstateclust1 <- merge(fullstateclust, covars, by.x = "state", by.y = "state")
dim(fullstateclust); dim(covars); dim(fullstateclust1)
fullstateclust1$statetreat <- fullstateclust1$statetreat.x
colnames(fullstateclust1)

fulltreat3 <- lm(statetreat ~ change0602, data=fullstateclust)
summary(fulltreat3)

#again, look at it proportionally too
fullstateclust$propchange0602 <- ifelse(fullstateclust$latvote02==0, 0, ((fullstateclust$latvote06 - fullstateclust$latvote02)/fullstateclust$latvote02))
mean(fullstateclust$propchange0602) #there are v. few for which turnout02 was 0, doesn't matter what I do with them.
fulltreat3a <- lm(statetreat ~ propchange0602, data=fullstateclust)
summary(fulltreat3a)

stargazer(treat3, treat3a)
stargazer(treat3, covariate.labels=c("2006 - 2002 Latino turnout (percentage points)"), 
	title= "Predicting treatment with prior Latino turnout trends (including all jurisdictions)",
	omit.stat=c("LL","ser","f", "AIC"), dep.var.labels.include = FALSE, 
	dep.var.caption= NULL, label="treatmentcheck1") #I think this is the one going in the paper. (table 7)

### but should produce this table for counties as well, although errors are going to be weird
for (i in 1:nrow(moadatadrops)){
moadatadrops$CVAP02alt[i] <-mean(c(moadatadrops$CVAP2002[i], moadatadrops$CVAP2006[i]))
moadatadrops$CVAP02alt1[i] <-((moadatadrops$CVAP2006[i] - moadatadrops$CVAP2002[i])/3)+moadatadrops$CVAP2002[i] #quick interpolation since "CVAP2002" is actually just the Census00 numbers.
moadatadrops$CVAP04alt[i] <-((moadatadrops$CVAP2006[i] - moadatadrops$CVAP2002[i])*(2/3))+moadatadrops$CVAP2002[i]
}

#actually just put a note, not the whole table.
#but also adjust by pop as above.
moadatadrops$change0206 <- moadatadrops$latinovote06 - moadatadrops$latinovote02
moadatadrops$change0206alt <- moadatadrops$latinoturnout06 - (moadatadrops$latinovote02/moadatadrops$CVAP02alt1)
moadatadrops$change0206alt <- moadatadrops$latinoturnout06 - (moadatadrops$latinovote02/moadatadrops$CVAP02alt1)

head(moadatadrops)

treat3 <- lm(constreat ~ change0206alt,  data = moadatadrops) 
summary(treat3) #need to cluster SE's, as in main analysis.

county <-ols(constreat ~ change0206alt,  data = moadatadrops,method="qr",x=T);
cluster_county<-robcov(county, moadatadrops$state, method=c('efron'));
print(cluster_county)

#also run a quick placebo test while we have the 02 data in here:
DDmainmlma1 <- glmer(change0206alt ~ constreat+(1+constreat | state), data=moadatadrops)
summary(DDmainmlma1)
DDmainmlma <- glmer(change0206alt ~ constreat+ had_senelection2006 + had_senelection2002 +had_govelection2006 +(1+constreat | state), data=moadatadrops)
summary(DDmainmlma)

DDmainmlmc <- glmer(change0206alt ~ constreat+ had_senelection2006 + had_senelection2002 +had_govelection2006 +(1 | state), data=moadatadrops)
summary(DDmainmlmc)

stargazer(DDmainmlma,DDmainmlmc, covariate.labels = c("Treatment (Involuntary SC enrollment)", "Senate election 2006", "Senate election 2002", "Governor election 2006"),title= "Placebo test: main analysis replicated on 2002-2006 treatment change",
	omit.stat=c("LL","ser","f", "AIC", "chi2"), label="mlmplacebo", dep.var.labels=c("Turnout change, 2006-2010") )

head(moadata)
moadata$latinovote02 <- moadata$Early02 + moadata$Mail02 + moadata$Polling02 + moadata$Absentee02 + moadata$Unknown02
moadata$change0206 <- moadata$latinovote06 - moadata$latinovote02

treat1 <- lm(constreat ~ change0206,  data = moadata) 
summary(treat1)


#also adjust by pop.
for (i in 1:nrow(moadata)){
moadata$CVAP02alt[i] <-mean(c(moadata$CVAP2002[i], moadata$CVAP2006[i]))
}

moadata$latinovote02 <- moadata$Early02 + moadata$Mail02 + moadata$Polling02 + moadata$Absentee02 + moadata$Unknown02
moadata$change0206pop <- moadata$latinoturnout06 - (moadata$latinovote02/moadata$CVAP2002)
moadata$change0206alt <- moadata$latinoturnout06 - (moadata$latinovote02/moadata$CVAP02alt)

treat1p <- lm(constreat ~ change0206pop,  data = moadata) 
summary(treat1p) #as noted above-- not sure this matters for this design.
treat1a <- lm(constreat ~ change0206alt,  data = moadata) 
summary(treat1a)

county <-ols(constreat ~ change0206alt,  data = moadata,method="qr",x=T);
cluster_county<-robcov(county, moadata$state, method=c('efron'));
print(cluster_county)


#############################################

## also rerun the treatment-prediction thing using the census data.
## even though this is fewer places.
moadata2 <- subset(moadata, is.na(moadata$CVAP2006)==F & 
	is.na(moadata$CVAP2002)==F & is.na(moadata$CVAP2010)==F)

moadata2$change0206 <- moadata2$latinoturnout06 - moadata2$latinoturnout02

ck1census <- lm(constreat~ change0206, data=moadata2)
summary(ck1census)

colnames(moadata2)
statecens <- ddply(moadata2, .(state), summarise, latpop06=sum(CVAP2006), 
	latpop10=sum(CVAP2010), latvote06 = sum(latinovote06), 
	latvote10 = sum(latinovote10), latpop02=sum(CVAP2002),latvote02 = sum(latinovote02))

statecens$constreat <- 0
statecens[statecens$state %in% c("Delaware", "Florida", "Virginia", "Texas", "West Virginia"), "constreat"]<- 1
statecens$change0206 <- (statecens$latvote06/statecens$latpop06) - (statecens$latvote02/statecens$latpop02)
ck2census <- lm(constreat~ change0206, data=statecens)
summary(ck2census)

##double-check that this holds for states if we drop the self-selected places.
moadata2 <- subset(moadatadrops1, is.na(moadatadrops1$CVAP2006)==F & 
	is.na(moadatadrops1$CVAP2002)==F & is.na(moadatadrops1$CVAP2010)==F)

moadata2$change0206 <- moadata2$latinoturnout06 - moadata2$latinoturnout02

ck1census <- lm(constreat~ change0206, data=moadata2)
summary(ck1census)

colnames(moadata2)
statecens <- ddply(moadata2, .(state), summarise, latpop06=sum(CVAP2006), 
	latpop10=sum(CVAP2010), latvote06 = sum(latinovote06), 
	latvote10 = sum(latinovote10), latpop02=sum(CVAP2002),latvote02 = sum(latinovote02))

statecens$constreat <- 0
statecens[statecens$state %in% c("Delaware", "Florida", "Virginia", "Texas", "West Virginia"), "constreat"]<- 1
statecens$change0206 <- (statecens$latvote06/statecens$latpop06) - (statecens$latvote02/statecens$latpop02)
ck2census <- lm(constreat~ change0206, data=statecens)
summary(ck2census)
library(apsrtable)
apsrtable(ck2census)

############
#also look at 2004-08 (another placebo test, though different election)
head(moadatadrops1)
moadatadrops$latinovote04 <- moadatadrops$Early04 + moadatadrops$Mail04 + moadatadrops$Polling04 + moadatadrops$Absentee04 + moadatadrops$Unknown04
moadatadrops$latinoturnout04 <- moadatadrops$latinovote04/ moadatadrops$CVAP04alt #(pop here is interp from Census00 and ACS06)
moadatadrops$latinoturnout04b <- moadatadrops$latinovote04/ moadatadrops$CVAP2006 #probably wronger but also allows more places to be included.

moadatadrops$change0804 <- moadatadrops$latinoturnout08 - moadatadrops$latinoturnout04
moadatadrops$change0804b <- moadatadrops$latinoturnout08 - moadatadrops$latinoturnout04b

#run it without clustering, etc, though that will understate SE's
placebo0408county<- lm(change0804 ~ constreat, data=moadatadrops)
summary(placebo0408county)

placebo0408county<- lm(change0804 ~ constreat + had_senelection2004 + had_senelection2008 + had_govelection2004 + had_govelection2008 , data=moadatadrops)
summary(placebo0408county)

#run the main analysis here
plac0408county<- glmer(change0804 ~ constreat + had_senelection2004 + had_senelection2008 + had_govelection2004  +(1+constreat|state), data=moadatadrops)
summary(plac0408county)

plac0408countyb<- glmer(change0804b ~ constreat + had_senelection2004 + had_senelection2008 + had_govelection2004  +(1+constreat|state), data=moadatadrops)
summary(plac0408countyb)
#nothing.

stargazer(plac0408county, plac0408countyb, covariate.labels = c("Treatment (Involuntary SC enrollment)", "Senate election 2004", 
	"Senate election 2008", "Governor election 2004", "Governor election 2008"), 
	title= "Placebo test: Using Treatment to Predict Latino Turnout Change 2004-2008",
	omit.stat=c("LL","ser","f", "AIC"), label="placebo2", dep.var.labels=c("Turnout change, 2004-2008")) 

dim(stateclust)
mean(na.omit(stateclust$latinoturnout10))


#####################################################################################
###################################################################################
## removals adjusted by time enrolled (presented in Appendix B)
head(moadata)

did2b <- lm(change ~ constreat + removalsadjusted + removalsadjusted*constreat, data=moadata) 
summary(did2b)

did2b <- lm(change ~ constreat + removalsadjusted + removalsadjusted*constreat, data=moadatadrops) 
summary(did2b)

did2b <- lm(change ~ constreat + submissionsadjusted + submissionsadjusted*constreat, data=moadata) 
summary(did2b)

did2b <- lm(change ~ constreat + submissionsadjusted + submissionsadjusted*constreat, data=moadatadrops) 
summary(did2b)


did2c <- lm(removalsadjusted ~ constreat + log(totalpop) + log(popdens), data=moadatadrops) 
summary(did2c) ##so treatment definitely does mean more removals by summer 2012; probably not by 2010?
did2d <- lm(submissionsadjusted ~ constreat + log(totalpop) + log(popdens), data=moadatadrops) 
summary(did2d) ##same deal with submissions.

submedian <- median(na.omit(moadatadrops$submissionsadjusted))
median(na.omit(moadatadrops$removalsadjusted))
max(na.omit(moadatadrops$submissionsadjusted))
max(na.omit(moadatadrops$removalsadjusted))

#keep places with high/low submissions (or else unenrolled)
highsub <- subset(moadatadrops, moadatadrops$submissionsadjusted > submedian | constreat==0) 
lowsub <-subset(moadatadrops, moadatadrops$submissionsadjusted <= submedian | constreat==0)
dim(highsub); dim(lowsub); dim(moadatadrops)
check <- subset(moadatadrops, constreat==1 &is.na(moadatadrops$submissionsadjusted)==T)
dim(check)

moadatadrops$submissions_popadj <- moadatadrops$submissionsadjusted*1000/moadatadrops$totalpop
submean <- mean(na.omit(moadatadrops$submissions_popadj))
treated <- subset(moadatadrops, moadatadrops$constreat==1)
dim(treated); median(treated$submissions_popadj)
submedian <- median(treated$submissions_popadj)

highsub <- subset(moadatadrops, moadatadrops$submissions_popadj > submedian | constreat==0) 
lowsub <-subset(moadatadrops, moadatadrops$submissions_popadj <= submedian | constreat==0)
mean(lowsub$submissions_popadj); mean(lowsub$submissionsadjusted)

dim(highsub); dim(lowsub); dim(moadatadrops)
dim(subset(highsub, highsub$constreat==1)); dim(subset(lowsub, lowsub$constreat==1))
dd2high <- lm(change ~ constreat + had_senelection2010 + had_senelection2006 + had_govelection2006 + had_govelection2010, data=highsub)
summary(dd2high)

dd2low <- lm(change ~ constreat + had_senelection2010 + had_senelection2006 + had_govelection2006 + had_govelection2010, data=lowsub)
summary(dd2low)

dd2ahigh <- lm(change ~ constreat, data=highsub)
summary(dd2ahigh)

dd2alow <- lm(change ~ constreat, data=lowsub)
summary(dd2alow)


#cluster SE's:
dd2high <- ols(change ~ constreat + had_senelection2010 + had_senelection2006 + had_govelection2006 + had_govelection2010, data=highsub,method="qr",x=T)
dd2high_clust <- robcov(dd2high, highsub$state, method=c('efron'));
print(dd2high_clust)

dd2low <- ols(change ~ constreat + had_senelection2010 + had_senelection2006 + had_govelection2006 + had_govelection2010, data=lowsub,method="qr",x=T)
dd2low_clust <- robcov(dd2low, lowsub$state, method=c('efron'));
print(dd2low_clust)

stargazer(dd2high_clust, dd2low_clust,  covariate.labels = c("Treatment (Involuntary SC enrollment)", "Senate election 2010", 
	"Senate election 2006", "Governor election 2006", "Governor election 2006"), 
	title= "Treatment effects by number of fingerprint submissions (Robust clustered SE's)",
	omit.stat=c("LL","ser","f", "AIC", "chi2"), label="highlow", dep.var.labels=c("Turnout change, 2006-2010"),
	column.labels=c("High submissions", "Low submissions"))


#should I adjust by total pop, or by Latino pop? (see below-- it really doesn't make much difference)
moadatadrops$submissions_popadj <- moadatadrops$submissionsadjusted*1000/moadatadrops$CVAP2010
submean <- mean(na.omit(moadatadrops$submissions_popadj))
treated <- subset(moadatadrops, moadatadrops$constreat==1)
dim(treated); median(treated$submissions_popadj)
submedian <- median(treated$submissions_popadj)

highsub <- subset(moadatadrops, moadatadrops$submissions_popadj > submedian | constreat==0) 
lowsub <-subset(moadatadrops, moadatadrops$submissions_popadj <= submedian | constreat==0)
mean(lowsub$submissions_popadj); mean(lowsub$submissionsadjusted)

dim(highsub); dim(lowsub); dim(moadatadrops)
dim(subset(highsub, highsub$constreat==1)); dim(subset(lowsub, lowsub$constreat==1))
dd2high <- lm(change ~ constreat + had_senelection2010 + had_senelection2006 + had_govelection2006 + had_govelection2010, data=highsub)
summary(dd2high)

dd2low <- lm(change ~ constreat + had_senelection2010 + had_senelection2006 + had_govelection2006 + had_govelection2010, data=lowsub)
summary(dd2low)

dd2ahigh <- lm(change ~ constreat, data=highsub)
summary(dd2ahigh)

dd2alow <- lm(change ~ constreat, data=lowsub)
summary(dd2alow)


mean(na.omit(moadatadrops$CVAP2010))*.18
treated <- subset(moadatadrops, moadatadrops$constreat==1)
mean(na.omit(treated$CVAP2010))*.16
median(na.omit(treated$CVAP2010))

###############################################
# synthetic control (appendix A)
###############################################
 library("Synth")

rm(list=ls())
#set up data (long format, reshaped in stata)
library(foreign)
longversion <- read.table("jurisdictiondata_patternseeking_LONGmidterms.csv", header=T, sep = "\t", stringsAsFactors=F, as.is=T)
head(longversion)
table(longversion$state) 

#merge in state MOA dates.
moadates <- read.csv("stateMOAdates.csv", stringsAsFactors=F)
colnames(moadates)
abbrevs <- read.csv("statenames.csv", stringsAsFactors=F) #add full names to MOA data
moanames <- merge(moadates, abbrevs, by.x="State", "abbrev")
dim(moadates); dim(abbrevs); dim(moanames)

longvers <- merge(x=longversion, y=moanames, by="state", all.x=T)
dim(longversion); dim(moanames); dim(longvers) #good, merge worked.
head(longvers); head(longversion)

##pull in counties dropped in the main analysis, to make this dataset match
maindrops <- read.csv("droppedcounties_fromcleanup.csv")
maindrops$combined <- paste(maindrops$abbrev, maindrops$county)#create combined variable
#create the same variable in longvers:
for (i in 1:nrow(longvers)){
	obs <- longvers$abbrev[i]
	longvers$abbrev[i]<- ifelse(obs=="", longvers$abbrev[i-1], obs)
}
longvers$combined <- paste(longvers$abbrev, longvers$county)
dim(longvers) #now drop the ones dropped in the other file (due to turnout>1)
longvers<- longvers[!(longvers$combined %in% maindrops$combined), ]
dim(longvers)
##note that this dataset is still a good bit smaller than the other one, as I've dropped places
## that are missing 2002 CVAP ests.

longvers$sigdate <- as.Date(longvers$administratorsignature, format="%m/%d/%y")
longvers$impdate <- as.Date(longvers$activationdate, format="%m/%d/%Y")
longvers$byelectionday <- ifelse(longvers$impdate <= "2010-11-02"  , 1, 0)

#set up treatment var
longvers$constreat <- 0

longvers[longvers$state=="Delaware","constreat"]<- 1
longvers[(longvers$state=="Florida" & longvers$impdate=="2010-06-22"),"constreat"]<- 1
longvers[(longvers$state=="Virginia" & longvers$impdate=="2010-06-15"),"constreat"]<- 1
longvers[(longvers$state=="West Virginia" & longvers$impdate=="2010-10-26"),"constreat"]<- 1
longvers[(longvers$state=="Texas" & longvers$impdate=="2010-09-28"),"constreat"]<- 1
sum(longvers$constreat)

longvers1 <- subset(longvers, (longvers$constreat==1)|(longvers$constreat == 0 & longvers$impdate>="2010-11-02")|(longvers$constreat == 0 & is.na(longvers$impdate)==T)) #include treated (pre-election) and untreated (never enrolled or enrolled after election).  
onlymids <- subset(longvers1, longvers1$Year%in%c(2002,2006,2010))
dim(longvers1); dim(onlymids)
sum(onlymids$state=="Virginia")	
colnames(onlymids)
head(onlymids); head(longvers1)
class(onlymids$CVAP)
#take this down to state-cluster level before doing synth (since state-cluster-level treatment)
library(plyr)
stateclust <- ddply(onlymids, .(state, Year), summarise, latvote = sum(latinovote), latpop =sum(CVAP), hadgov = max(had_govelection), hadsen=max(had_senelection))
head(stateclust)
#now create turnout
stateclust$latinoturnout <- stateclust$latvote / stateclust$latpop
i<- 2
head(stateclust);
for (i in 1:nrow(stateclust)){ 
	year <- stateclust[i, "Year"]
	value <-if (year==2006){(stateclust[i, "latinoturnout"] - stateclust[i-1, "latinoturnout"])}else {NA}
	stateclust[i, "change"]<- value
}



# construct synth control for each state separately. start with VA.
othersVA <- c("Delaware", "Florida", "Texas", "West Virginia")
stateclustVA1 <- subset(stateclust, !(stateclust$state %in% othersVA))
stateclustVA <- subset(stateclustVA1, is.na(stateclustVA1$latinoturnout)==F)
stateclustVA$code <- as.numeric(as.factor(stateclustVA$state))
dim(stateclustVA); dim(stateclust)
stateclustVA[100:125,]

#now DE
othersDE <- c("Virginia", "Florida", "Texas", "West Virginia")
stateclustDE1 <- subset(stateclust, !(stateclust$state %in% othersDE))
stateclustDE <- subset(stateclustDE1, is.na(stateclustDE1$latinoturnout)==F)
stateclustDE$code <- as.numeric(as.factor(stateclustDE$state))
dim(stateclustDE); dim(stateclust)

#now TX
othersTX <- c("Virginia", "Florida", "Delaware", "West Virginia")
stateclustTX1 <- subset(stateclust, !(stateclust$state %in% othersTX))
stateclustTX <- subset(stateclustTX1, is.na(stateclustTX1$latinoturnout)==F)
stateclustTX$code <- as.numeric(as.factor(stateclustTX$state))
dim(stateclustTX); dim(stateclust)


#now WV
othersWV <- c("Virginia", "Florida", "Texas", "Delaware")
stateclustWV1 <- subset(stateclust, !(stateclust$state %in% othersWV))
stateclustWV <- subset(stateclustWV1, is.na(stateclustWV1$latinoturnout)==F)

stateclustWV$code <- as.numeric(as.factor(stateclustWV$state))
dim(stateclustWV); dim(stateclust)

#now FL
othersFL <- c("Virginia", "Delaware", "Texas", "West Virginia")
stateclustFL1 <- subset(stateclust, !(stateclust$state %in% othersFL))
stateclustFL <- subset(stateclustFL1, is.na(stateclustFL1$latinoturnout)==F)

stateclustFL$code <- as.numeric(as.factor(stateclustFL$state))
dim(stateclustFL); dim(stateclust)

statenames <- stateclust$state
statenamesVA <- unique(stateclustVA$state[stateclustVA$state!="Virginia"])
statenamesDE<- unique(stateclustDE$state[stateclustDE$state!="Delaware"])
statenamesFL <- unique(stateclustFL$state[stateclustFL$state!="Florida"])
statenamesTX <- unique(stateclustTX$state[stateclustTX$state!="Texas"])
statenamesWV <- unique(stateclustWV$state[stateclustWV$state!="West Virginia"])


###################################################
### (much of this code taken from Abadie et al example code)
###################################################
head(stateclustVA)

#VA
 dataprep.out.VA <-
              dataprep(foo = stateclustVA,
                       predictors = c() ,
                       predictors.op = "mean" ,
                      time.predictors.prior = seq(2002, 2006, 4),
                       special.predictors = list(
			list("change", 2006 , "mean"), 
			list("hadgov", 2002, "mean"),
			list("hadgov", 2006, "mean"),
			list("hadsen", 2002, "mean"),
			list("hadsen", 2006, "mean")),
                       dependent = "latinoturnout",
                      unit.variable = "code",
                       unit.names.variable = "state",
                       time.variable = "Year",
                       treatment.identifier = "Virginia", 
                       controls.identifier = statenamesVA,
                       time.optimize.ssr = seq(2002, 2006, 4),
                       time.plot = seq(2002, 2010, 4)
                       )

#DE
 dataprep.out.DE <-
              dataprep(foo = stateclustDE,
                       predictors = c() ,
                       predictors.op = "mean" ,
                      time.predictors.prior = seq(2002, 2006, 4),
                       special.predictors = list(
			list("change", 2006 , "mean"), 
			list("hadgov", 2002, "mean"),
			list("hadgov", 2006, "mean"),
			list("hadsen", 2002, "mean"),
			list("hadsen", 2006, "mean")),
                       dependent = "latinoturnout",
                      unit.variable = "code",
                       unit.names.variable = "state",
                       time.variable = "Year",
                       treatment.identifier = "Delaware", 
                       controls.identifier = statenamesDE,
                       time.optimize.ssr = seq(2002, 2006, 4),
                       time.plot = seq(2002, 2010, 4)
                       )
#FL
 dataprep.out.FL <-
              dataprep(foo = stateclustFL,
                       predictors = c() ,
                       predictors.op = "mean" ,
                      time.predictors.prior = seq(2002, 2006, 4),
                       special.predictors = list(
			list("change", 2006 , "mean"), 
			list("hadgov", 2002, "mean"),
			list("hadgov", 2006, "mean"),
			list("hadsen", 2002, "mean"),
			list("hadsen", 2006, "mean")),
                       dependent = "latinoturnout",
                      unit.variable = "code",
                       unit.names.variable = "state",
                       time.variable = "Year",
                       treatment.identifier = "Florida", 
                       controls.identifier = statenamesFL,
                       time.optimize.ssr = seq(2002, 2006, 4),
                       time.plot = seq(2002, 2010, 4)
                       )

#TX
 dataprep.out.TX <-
              dataprep(foo = stateclustTX,
                       predictors = c() ,
                       predictors.op = "mean" ,
                      time.predictors.prior = seq(2002, 2006, 4),
                       special.predictors = list(
			list("change", 2006 , "mean"), 
			list("hadgov", 2002, "mean"),
			list("hadgov", 2006, "mean"),
			list("hadsen", 2002, "mean"),
			list("hadsen", 2006, "mean")),
                       dependent = "latinoturnout",
                      unit.variable = "code",
                       unit.names.variable = "state",
                       time.variable = "Year",
                       treatment.identifier = "Texas", 
                       controls.identifier = statenamesTX,
                       time.optimize.ssr = seq(2002, 2006, 4),
                       time.plot = seq(2002, 2010, 4)
                       )
#WV
 dataprep.out.WV <-
              dataprep(foo = stateclustWV,
                       predictors = c() ,
                       predictors.op = "mean" ,
                      time.predictors.prior = seq(2002, 2006, 4),
                       special.predictors = list(
			list("change", 2006 , "mean"), 
			list("hadgov", 2002, "mean"),
			list("hadgov", 2006, "mean"),
			list("hadsen", 2002, "mean"),
			list("hadsen", 2006, "mean")),
                       dependent = "latinoturnout",
                      unit.variable = "code",
                       unit.names.variable = "state",
                       time.variable = "Year",
                       treatment.identifier = "West Virginia",
                       controls.identifier = statenamesWV,
                       time.optimize.ssr = seq(2002, 2006, 4),
                       time.plot = seq(2002, 2010, 4)
                       )

###################################################
 synth.out.VA <- synth(data.prep.obj = dataprep.out.VA,
                    method = "BFGS")
	
 synth.out.DE <- synth(data.prep.obj = dataprep.out.DE,
                    method = "BFGS")
 synth.out.FL <- synth(data.prep.obj = dataprep.out.FL,
                    method = "BFGS")
 synth.out.TX <- synth(data.prep.obj = dataprep.out.TX,
                    method = "BFGS")
 synth.out.WV <- synth(data.prep.obj = dataprep.out.WV,
                    method = "BFGS")



###################################################
 synth.tables.VA <- synth.tab(dataprep.res = dataprep.out.VA,
                           synth.res = synth.out.VA
                           )
 synth.tables.DE <- synth.tab(dataprep.res = dataprep.out.DE,
                           synth.res = synth.out.DE
                           )
 synth.tables.FL <- synth.tab(dataprep.res = dataprep.out.FL,
                           synth.res = synth.out.FL
                           )
 synth.tables.TX <- synth.tab(dataprep.res = dataprep.out.TX,
                           synth.res = synth.out.TX
                           )
 synth.tables.WV <- synth.tab(dataprep.res = dataprep.out.WV,
                           synth.res = synth.out.WV
                           )



##now figure out DD:
statedatVA <- merge(stateclustVA, synth.tables.VA$tab.w, by.x="state", by.y="unit.names", all.x=T) 

VA <- stateclustVA[stateclustVA$state=="Virginia",]
synthset.VA <- statedatVA[statedatVA$w.weights>0 & is.na(statedatVA$w.weights)==F,]
synthyears.VA <- ddply(synthset.VA, .(Year), summarise, latinoturnout = weighted.mean(latinoturnout, w.weights), 
	hadgov = weighted.mean(hadgov, w.weights), hadsen = weighted.mean(hadsen, w.weights))
VA2010 <- VA[VA$Year==2010,]
VA2006 <-VA[VA$Year==2006,]
synth2010VA <- synthyears.VA[synthyears.VA$Year==2010,]
synth2006VA <- synthyears.VA[synthyears.VA$Year==2006,]
DD.VA <- (VA2010$latinoturnout - VA2006$latinoturnout)-(synth2010VA$latinoturnout - synth2006VA$latinoturnout)

#DE
statedatDE <- merge(stateclustDE, synth.tables.DE$tab.w, by.x="state", by.y="unit.names", all.x=T) 
DE <- stateclustDE[stateclustDE$state=="Delaware",]
synthset.DE <- statedatDE[statedatDE$w.weights>0 & is.na(statedatDE$w.weights)==F,]
synthyears.DE <- ddply(synthset.DE, .(Year), summarise, latinoturnout = weighted.mean(latinoturnout, w.weights), 
	hadgov = weighted.mean(hadgov, w.weights), hadsen = weighted.mean(hadsen, w.weights))
DE2010 <- DE[DE$Year==2010,]
DE2006 <-DE[DE$Year==2006,]
synth2010DE <- synthyears.DE[synthyears.DE$Year==2010,]
synth2006DE <- synthyears.DE[synthyears.DE$Year==2006,]
DD.DE <- (DE2010$latinoturnout - DE2006$latinoturnout)-(synth2010DE$latinoturnout - synth2006DE$latinoturnout)

#FL
statedatFL <- merge(stateclustFL, synth.tables.FL$tab.w, by.x="state", by.y="unit.names", all.x=T) 
FL <- stateclustFL[stateclustFL$state=="Florida",]
synthset.FL <- statedatFL[statedatFL$w.weights>0 & is.na(statedatFL$w.weights)==F,]
synthyears.FL <- ddply(synthset.FL, .(Year), summarise, latinoturnout = weighted.mean(latinoturnout, w.weights), 
	hadgov = weighted.mean(hadgov, w.weights), hadsen = weighted.mean(hadsen, w.weights))
FL2010 <- FL[FL$Year==2010,]
FL2006 <-FL[FL$Year==2006,]
synth2010FL <- synthyears.FL[synthyears.FL$Year==2010,]
synth2006FL <- synthyears.FL[synthyears.FL$Year==2006,]
DD.FL <- (FL2010$latinoturnout - FL2006$latinoturnout)-(synth2010FL$latinoturnout - synth2006FL$latinoturnout)

#TX
statedatTX <- merge(stateclustTX, synth.tables.TX$tab.w, by.x="state", by.y="unit.names", all.x=T) 
TX <- stateclustTX[stateclustTX$state=="Texas",]
synthset.TX <- statedatTX[statedatTX$w.weights>0 & is.na(statedatTX$w.weights)==F,]
synthyears.TX <- ddply(synthset.TX, .(Year), summarise, latinoturnout = weighted.mean(latinoturnout, w.weights), 
	hadgov = weighted.mean(hadgov, w.weights), hadsen = weighted.mean(hadsen, w.weights))
TX2010 <- TX[TX$Year==2010,]
TX2006 <-TX[TX$Year==2006,]
synth2010TX <- synthyears.TX[synthyears.TX$Year==2010,]
synth2006TX <- synthyears.TX[synthyears.TX$Year==2006,]
DD.TX <- (TX2010$latinoturnout - TX2006$latinoturnout)-(synth2010TX$latinoturnout - synth2006TX$latinoturnout)

#WV
statedatWV <- merge(stateclustWV, synth.tables.WV$tab.w, by.x="state", by.y="unit.names", all.x=T) 
WV <- stateclustWV[stateclustWV$state=="West Virginia",]
synthset.WV <- statedatWV[statedatWV$w.weights>0 & is.na(statedatWV$w.weights)==F,]
synthyears.WV <- ddply(synthset.WV, .(Year), summarise, latinoturnout = weighted.mean(latinoturnout, w.weights), 
	hadgov = weighted.mean(hadgov, w.weights), hadsen = weighted.mean(hadsen, w.weights))
WV2010 <- WV[WV$Year==2010,]
WV2006 <-WV[WV$Year==2006,]
synth2010WV <- synthyears.WV[synthyears.WV$Year==2010,]
synth2006WV <- synthyears.WV[synthyears.WV$Year==2006,]
DD.WV <- (WV2010$latinoturnout - WV2006$latinoturnout)-(synth2010WV$latinoturnout - synth2006WV$latinoturnout)


DD.DE; DD.FL; DD.VA; DD.TX; DD.WV
plainmean <- mean(c(DD.VA, DD.DE, DD.FL, DD.TX, DD.WV)) 

totalpop2006 <- sum(stateclust[(stateclust$state %in% c("Delaware", "Florida", "Virginia", "Texas", "West Virginia") & stateclust$Year==2006), "latpop"])
statespop <- c(stateclust[(stateclust$state %in% c("Delaware") & stateclust$Year==2006), "latpop"],
stateclust[(stateclust$state %in% c("Florida") & stateclust$Year==2006), "latpop"],
stateclust[(stateclust$state %in% c("Virginia") & stateclust$Year==2006), "latpop"],
stateclust[(stateclust$state %in% c("Texas") & stateclust$Year==2006), "latpop"],
stateclust[(stateclust$state %in% c("West Virginia") & stateclust$Year==2006), "latpop"])
stateweights <- statespop/totalpop2006
weightedmeanbypop <- weighted.mean(c(DD.VA, DD.DE, DD.FL, DD.TX, DD.WV), stateweights) 

head(stateclust)
#now weight by #units as in other analysis, not pop.
stateunits <- c(3,43,25,46,54) #from sample table generated in other script/data, since it's more of a hassle in longform
totalunits <- sum(stateunits)
stateunitweights <- stateunits/totalunits
weightedmeanbyunits <- weighted.mean(c(DD.VA, DD.DE, DD.FL, DD.TX, DD.WV), stateunitweights)

ddests <- c(DD.DE, DD.FL, DD.VA, DD.TX, DD.WV, plainmean, weightedmeanbypop, weightedmeanbyunits)
names <- c("Delaware", "Florida", "Virginia", "Texas", "West Virginia", "Mean", "Population-weighted Mean", "Unit-weighted Mean")
esttable <- cbind(as.data.frame(names), as.data.frame(ddests))
library(xtable)
print(xtable(esttable, label="synthests", caption="Difference-in-difference estimates, compared to synthetic versions of each cluster",
digits=4), include.rownames=FALSE, digits=4)



###############################################################################################################
###############################################################################################################
### BELOW: CODE FOR STATE JACKKNIFE (FIGURE 3 IN PAPER)
### CAN RUN CODE FROM HERE WITHOUT RUNNING THE ABOVE
### NOTE THAT ESTS MAY DIFFER SLIGHTLY FROM PAPER DUE TO PACKAGE UPDATES
###############################################################################################################
###############################################################################################################
##PULL IN DATA AND SET UP
rm(list=ls())
setwd("/home/ariel/Desktop/SCanalysis/replicationdata")
library(foreign)
longdata <- read.csv("jurisdictiondata_incwhites.csv", sep = "\t", stringsAsFactors=F)
longdatafull<- longdata #save before dropping stuff

mean(na.omit(longdata$latinoturnout10)); max(na.omit(longdata$latinoturnout10)) 
longdata$blackturnout10 <- longdata$blackvoters10 / longdata$blackVAP2010
longdata$blackturnout06 <- longdata$blackvoters06 / longdata$blackVAP2006
longdata$whiteturnout10 <- longdata$whitevoters10 / longdata$whiteVAP2010
longdata$whiteturnout06 <- longdata$whitevoters06 / longdata$whiteVAP2006


#drop nonsense (need to decide how to do this):
dim(longdata)
sum(is.na(longdata$change)) #why so many missing still?
#longdata$change <- ifelse(longdata$latinoturnout10>1, NA, longdata$change)
#longdata$change <- ifelse(longdata$latinoturnout06>1, NA, longdata$change)
longdata[is.infinite(longdata$whiteturnout10)==T,"whiteturnout10"]<- NA
longdata[is.infinite(longdata$whiteturnout06)==T,"whiteturnout06"]<- NA
longdata[is.infinite(longdata$blackturnout10)==T,"blackturnout10"]<- NA
longdata[is.infinite(longdata$blackturnout06)==T,"blackturnout06"]<- NA
longdata[is.infinite(longdata$latinoturnout10)==T,"latinoturnout10"]<- NA
longdata[is.infinite(longdata$latinoturnout06)==T,"latinoturnout06"]<- NA
for (i in 1:nrow(longdata)){ #should clean up to run faster.
	problem10 <- ifelse(longdata$latinoturnout10[i]>1, 1, 0)
	longdata$latinoturnout10[i] <- ifelse(problem10==1, NA, longdata$latinoturnout10[i])
	longdata$latinovote10[i] <- ifelse(problem10==1, NA,longdata$latinovote10[i])
	longdata$CVAP2010[i] <- ifelse(problem10==1, NA,longdata$CVAP2010[i])
	problem08 <- ifelse(longdata$latinoturnout08[i]>1, 1, 0)
	longdata$latinoturnout08[i] <- ifelse(problem08==1, NA, longdata$latinoturnout08[i])
	longdata$latinovote08[i] <- ifelse(problem08==1, NA,longdata$latinovote08[i])
	longdata$CVAP2008[i] <- ifelse(problem08==1, NA,longdata$CVAP2008[i])
	problem06 <- ifelse(longdata$latinoturnout06[i]>1, 1, 0)
	longdata$latinoturnout06[i] <- ifelse(problem06==1, NA, longdata$latinoturnout06[i])
	longdata$latinovote06[i] <- ifelse(problem06==1, NA,longdata$latinovote06[i])
	longdata$CVAP2006[i] <- ifelse(problem06==1, NA,longdata$CVAP2006[i])
}



longdata$change <- longdata$latinoturnout10 - longdata$latinoturnout06

dim(longdata) #how many dropped?
sum(is.na(longdata$change)) #only about 50.

#merge in state MOA dates.
moadates <- read.csv("/home/ariel/Desktop/thinking/SecureCommunitiesEnrollment/stateMOAdates.csv", stringsAsFactors=F)
colnames(longdata); colnames(moadates)
abbrevs <- read.csv("statenames.csv", stringsAsFactors=F) #add full names to MOA data
moanames <- merge(moadates, abbrevs, by.x="State", "abbrev")
dim(moadates); dim(abbrevs); dim(moanames)
moadata <- merge(x=longdata, y=moanames, by.x="state", by.y="state", all.x=T)
dim(longdata); dim(moadates); dim(moadata) #good, merge worked.

moadata$sigdate <- as.Date(moadata$administratorsignature, format="%m/%d/%y")
moadata$impdate <- as.Date(moadata$activationdate, format="%m/%d/%Y")
moadata$byelectionday <- ifelse(moadata$impdate <= "2010-11-02"  , 1, 0)
moadata$dateofstats <- as.Date("08/31/12", format="%m/%d/%y")
moadata$timeenroll <- moadata$dateofstats - moadata$impdate
head(moadata$timeenroll)
moadata$imptoelection <- as.numeric(as.Date("2010-11-02") - moadata$impdate)
moadata$imptoelection <- ifelse(moadata$imptoelection<0, NA, moadata$imptoelection) ##no obs for places not enrolled by election day.
moadata$removalsadjusted <- (as.numeric(gsub(",", "", moadata$removals_total)) / as.numeric(moadata$timeenroll))*moadata$imptoelection
moadata$submissionsadjusted <- (as.numeric(gsub(",", "", moadata$submissions)) / as.numeric(moadata$timeenroll))*moadata$imptoelection
length(moadata$submissionsadjusted); sum(is.na(moadata$submissionsadjusted))
#subset(moadata$submissionsadjusted, is.na(moadata$submissionsadjusted)==F)
write.csv(moadata, file="MOAmergedjurdata_march2013.csv")

colnames(moadata)
colnames(moadata)[colnames(moadata)=="P0010001"] <- "totalpop"
colnames(moadata)[colnames(moadata)=="T002_002"] <- "popdens"
colnames(moadata)[colnames(moadata)=="T002_006"] <- "landarea"

##should also merge in income.

##also, merge moadates with a list of states to have a list of all states and status.
library(foreign)
abbrevs <- read.csv("statenames.csv", stringsAsFactors=F)
moalist <- merge(x=moadates, y=abbrevs, by.x="State", by.y="abbrev", all.y=T)
dim(moadates); dim(abbrevs); dim(moalist)

moalist$admdate <- as.Date(moalist$administratorsignature, format="%m/%d/%y")
moalist$icedate <- as.Date(moalist$ICEsignature, format="%m/%d/%y")
moalist$byelectionday <- ifelse(moalist$admdate < "2010-11-02" , 2, 1)
moalist$byelectionday[is.na(moalist$byelectionday)] <- 1 

####################################################################################
#set up very conservative treatment var (last set of counties to enroll in state)
moadata$constreat <- 0

moadata[moadata$state=="Delaware","constreat"]<- 1
moadata[(moadata$state=="Florida" & moadata$impdate=="2010-06-22"),"constreat"]<- 1
moadata[(moadata$state=="Virginia" & moadata$impdate=="2010-06-15"),"constreat"]<- 1
moadata[(moadata$state=="West Virginia" & moadata$impdate=="2010-10-26"),"constreat"]<- 1
moadata[(moadata$state=="Texas" & moadata$impdate=="2010-09-28"),"constreat"]<- 1
sum(moadata$constreat)

moadatadrops <- subset(moadata, (moadata$constreat==1)|(moadata$constreat == 0 & moadata$impdate>="2010-11-02")|(moadata$constreat == 0 & is.na(moadata$impdate)==T)) #include treated (pre-election) and untreated (never enrolled or enrolled after election).  Is there a better way to do this?
dim(moadatadrops); dim(moadata)



head(longdatafull)
for (i in 1:nrow(longdatafull)){
	obs <- longdatafull$abbrev[i]
	longdatafull$abbrev[i]<- ifelse(obs=="", longdatafull$abbrev[i-1], obs)
}

############################################################################################
## Run HLM

library(arm)
moadatadrops1 <- moadatadrops
DDsimpmlm <- glmer(change ~ constreat+(1+constreat | state), data=moadatadrops1)
summary(DDsimpmlm) #pull out coeffs to plot, as in VID paper.

DDmainmlm <- glmer(change ~ constreat+ had_senelection2006 + had_senelection2010 +had_govelection2006 +had_govelection2010+(1+constreat | state), data=moadatadrops1)
summary(DDmainmlm)

origcoef <-  fixef(DDmainmlm)[2]

###############################################################################################################
### RERUN PREFERRED SPECIFICATION WITHOUT EACH JURISDICTION.
###############################################################################################################

states1 <- unique(moadatadrops1$state)
states <- states1[is.na(states1)==F & states1!="Utah"] #Run Utah separately due to error (see below, looks fine)
savedcoeffs <- as.data.frame(matrix(nrow=length(states), ncol=2))
for (i in 1:nrow(savedcoeffs)){
	savedcoeffs[i,1]<- drop <- states[i]
	newdata <- subset(moadatadrops1, moadatadrops1$state != drop) #create new dataset without one state
	DDmlm <- glmer(change ~ constreat+ had_senelection2006 + had_senelection2010 +had_govelection2006 +had_govelection2010+(1+constreat | state), data=newdata)
	savedcoeffs[i,2] <- fixef(DDmlm)[2] #extract coefficient and save.
}
#sort these and plot.
sorted <- sort(savedcoeffs[,2])
ordered <- savedcoeffs[with(savedcoeffs, order(V2)),]
pdf("coefficient_statejackknife_mlm.pdf")
plot(ordered$V2, ylim=c(0,.05), ylab="Coefficient Estimate", xaxt="n", xlab=NA, main="Estimated Effects, One State Dropped")
abline(h=origcoef, lty=3)
axis(1, at = seq(1, nrow(ordered), by = 1), labels=ordered$V1, las=2, cex.axis=.55)
dev.off()

sum(moadatadrops1$state=="Utah")
moadatadrops2 <- moadatadrops1[moadatadrops1$state!="Utah",]
DDmainmlmU <- glmer(change ~ constreat+ had_senelection2006 + had_senelection2010 +had_govelection2006 + (1+constreat | state), data=moadatadrops2)
summary(DDmainmlmU)
sum(moadatadrops2$constreat)
dim(moadatadrops2)

DDmainlmU <- lm(change ~ constreat+ had_senelection2006 + had_senelection2010 +had_govelection2006 , data=moadatadrops2)
summary(DDmainlmU)






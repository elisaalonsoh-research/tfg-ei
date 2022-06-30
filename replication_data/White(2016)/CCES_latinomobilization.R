### Ariel White
### replication code for "When Threat Mobilizes"
### this script produces CCES analysis in the paper/appendix C
### spring 2016

rm(list=ls())
setwd("/home/ariel/Desktop/SCanalysis/replicationdata")

library(foreign)
cces <- read.dta("cces_2010_common_validated.dta")
head(cces)
#subset to Hispanic/Latino respondents
latinos <- subset(cces, as.character(cces$V211)=="Hispanic" | as.character(cces$V290)=="Yes")
dim(latinos)
latinosmall <- latinos[,c("V211", "V290", "CC425a", "V277", "V278", "V302", "V207", "V212a", "V214", "V208", "V203", "V101", "CC401", "vote_gen10")]
head(latinosmall)
latinosmall$contact <- 2-as.numeric(latinosmall$CC425a) #want 1 if yes, 0 if no.
mean(latinosmall$contact, na.rm=T)
latinosmall$votereport <- 0
latinosmall$votereport<- ifelse(as.numeric(latinosmall$CC401)==5, 1, 0)
#okay, now need to merge in treated info using fips codes.
treatment <- read.csv("treatmentstatusofcounties.csv")
head(treatment)
treatment$County <- paste(treatment$county, treatment$State)
treatmentlong <- treatment
treatment<- treatment[,c("constreat", "County", "GEOid2")]

ccestreat <- merge(latinosmall, treatment, by.x="V278", by.y="County", all.x=T) #check this.
dim(treatment); dim(latinosmall); dim(ccestreat)
head(ccestreat)
ccestreat[is.na(ccestreat$constreat)==T,"constreat"]<- 0 #which are these? (note that results don't depend on these places.
mean(ccestreat$constreat)
ccestreat1<- ccestreat

#okay, also merge in state election covariates
covars <- read.csv("statecovariates.csv", stringsAsFactors=F)
elections10 <- covars[,c("abbrev", "state", "had_senelection2010", "had_govelection2010")]
elections06 <- covars[,c("abbrev", "state", "had_senelection2006", "had_govelection2006")]

sum(is.na(ccestreat1$State)); sum(is.na(ccestreat1$state)); sum(is.na(ccestreat1$stateid))

ccestreat1$otherstate <- NA
for (i in 1:nrow(ccestreat1)){
	row <- ccestreat1[i,]
	state <- substr(row$V278, nchar(row$V278)-1, nchar(row$V278))
	ccestreat1$otherstate[i] <- state
}
head(ccestreat1)
sum(is.na(ccestreat1$otherstate))
ccestreat <- merge(ccestreat1, elections10, by.x = "otherstate", by.y = "abbrev")
dim(ccestreat1); dim(elections10); dim(ccestreat)
head(ccestreat) 

treat<-subset(ccestreat, ccestreat$constreat==1)
untreat <- subset(ccestreat, ccestreat$constreat==0)
mean(treat$contact, na.rm=T); mean(untreat$contact, na.rm=T)
t.test(treat$contact, untreat$contact, alternative=c("greater"))
head(ccestreat)
ccestreat$registered <- 2-as.numeric(ccestreat$V203)
ccestreat$age <- 2010-as.numeric(ccestreat$V207)
contact0.0 <- lm(contact ~ constreat, data=ccestreat)
summary(contact0.0)
contact0 <- lm(contact ~ constreat, data=ccestreat, weights=V101)
summary(contact0)
contact1 <- lm(contact ~ constreat + registered + age + V208, data=ccestreat, weights=V101)
summary(contact1)
contact2 <- lm(contact ~ constreat + state + registered + age + V208 + V212a + had_govelection2010 + had_senelection2010, data=ccestreat, weights=V101)
summary(contact2)
contact3 <- lm(contact ~ constreat +  registered + age + V208 + V212a + had_govelection2010 + had_senelection2010, data=ccestreat, weights=V101)
summary(contact3)

##should also look at reported voting.
votereport <- lm(votereport ~ constreat, data=ccestreat, weights=V101)
summary(votereport)

votereport1 <- lm(votereport ~ constreat+ registered + age + V208 + V212a, data=ccestreat, weights=V101)
summary(votereport1)

votereport2 <- lm(votereport ~ constreat+ registered + age + V208 + V212a + had_govelection2010 + had_senelection2010, data=ccestreat, weights=V101)
summary(votereport2)

require(stargazer)
stargazer(contact0, contact1, votereport, votereport1, label="CCES2010full", omit.stat=c("LL","ser","f", "AIC", "chi2"))
stargazer(contact0, contact1, contact3, label="CCES2010full", omit.stat=c("LL","ser","f", "AIC", "chi2"))

#########################3333
#placebo: do we see this in 2006? 
cces06 <- read.table("cces_2006_common.tab", header=T, as.is=T, sep="\t", fill=T)
head(cces06)
latinos06 <- subset(cces06, cces06$v2005==3| cces06$v2009==1)
dim(latinos06); dim(cces06)
latinosmall06 <- latinos06[,c("v1001","v2005", "v2009", "v1002", "v1004", "v4065", "v3004", "v4034", "v2004", "v2019", "v2020", "v3005", "vote06turn", "v4004", "v4005")] #add in other contextual vars here later.
head(latinosmall06)
latinosmall06$contact <- 2-as.numeric(latinosmall06$v4065) #want 1 if yes, 0 if no.
latinosmall06$registered <- 2-as.numeric(latinosmall06$v3004)
latinosmall06$age <- 2006-latinosmall06$v2020
latinosmall06$vote <- 0
latinosmall06[latinosmall06$v4004==1 | latinosmall06$v4005==4, "vote"]<- 1
mean(latinosmall06$contact, na.rm=T)
head(treatment)
#merge
ccestreat06 <- merge(latinosmall06, treatmentlong, by.x="v1004", by.y="GEOid2", all.x=T) #check this.
dim(treatmentlong); dim(latinosmall06); dim(ccestreat06) #actually almost all of them merge.
ccestreat06_1 <- ccestreat06

ccestreat06 <- merge(ccestreat06_1, elections06, by.x = "state", by.y = "state")
head(ccestreat06)

contact0_06 <- lm(contact ~ constreat, data=ccestreat06)
summary(contact0_06)

contact1_06 <- lm(contact ~ constreat + registered + v2004 + age, data=ccestreat06, weights=as.numeric(v1001))
summary(contact1_06)
#2004 = gender; 3005=3pt party id

contact2_06 <- lm(contact ~ constreat + registered + v2004 + age + v3005 + State, data=ccestreat06, weights=as.numeric(v1001))
summary(contact2_06)


contact3_06 <- lm(contact ~ constreat + registered + v2004 + age + v3005 + had_senelection2006 +had_govelection2006, data=ccestreat06, weights=as.numeric(v1001))
summary(contact3_06)

##should also look at reported voting.
head(ccestreat06)
vote06 <-  lm(vote ~ constreat + registered + v2004 + age, data=ccestreat06, weights=as.numeric(v1001))
summary(vote06)

stargazer(contact0_06, contact1_06, vote06, label="CCES2006full", omit.stat=c("LL","ser","f", "AIC", "chi2"))

############################################################################
## To present in paper: run everything with only the sample from the paper
## (dropping self-selected counties).

drops <- read.csv("droppedcounties.csv")
drops$County <- paste(drops$county, drops$abbrev)
#okay,copy-paste the analysis from above, but drop these counties.

head(ccestreat)
ccesdrops <- subset(ccestreat, !(ccestreat$V278 %in% drops$County))
dim(ccestreat); dim(ccesdrops)
contact0.0 <- lm(contact ~ constreat, data=ccesdrops)
summary(contact0.0)
contact0 <- lm(contact ~ constreat, data=ccesdrops, weights=V101)
summary(contact0)
contact1 <- lm(contact ~ constreat + registered + age + V208, data=ccesdrops, weights=V101)
summary(contact1)
#v208 = gender, v212a=3pt party id
contact2 <- lm(contact ~ constreat + registered + age + V208 + V212a, data=ccesdrops, weights=V101)
summary(contact2)

contact3 <- lm(contact ~ constreat + registered + age + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops, weights=V101)
summary(contact3)

##should also look at reported voting.
votereport <- lm(votereport ~ constreat, data=ccesdrops, weights=V101)
summary(votereport)


votereport1 <- lm(votereport ~ constreat+ registered + age + V208 + V212a, data=ccesdrops, weights=V101)
summary(votereport1)

require(stargazer)
stargazer(contact0, contact1, contact3, label="CCES2010drops", omit.stat=c("LL","ser","f", "AIC", "chi2"))

##and now cluster at the county level?
library(rms)
contact0 <- ols(contact ~ constreat, data=ccesdrops, weights=V101, x=T)
cluster_contact0<-robcov(contact0, ccesdrops$V278, method=c('efron'));
print(cluster_contact0)

contact1 <- ols(contact ~ constreat + registered + age + V208, data=ccesdrops, weights=V101, x=T)
cluster_contact1<-robcov(contact1, ccesdrops$V278, method=c('efron'));
print(cluster_contact1)

#v208 = gender, v212a=3pt party id
contact2 <- ols(contact ~ constreat + registered + age + V208 + V212a, data=ccesdrops, weights=V101, x=T)
cluster_contact2<-robcov(contact2, ccesdrops$V278, method=c('efron'));
print(cluster_contact2)

contact3 <- ols(contact ~ constreat + registered + age + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops, weights=V101, x=T)
cluster_contact3<-robcov(contact3, ccesdrops$V278, method=c('efron'));

print(cluster_contact3)

contact4 <- lm(contact ~ constreat + registered + age + I(age^2) + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops, weights=V101, x=T)
summary(contact4)

require(stargazer)
stargazer(cluster_contact0, cluster_contact1, cluster_contact3, label="CCES2010drops_clust",
	 omit.stat=c("LL","ser","f", "AIC", "chi2"),
	covariate.labels=c("SC Treatment", "Registered Voter", "Gender: Female", "Age", "Party ID:Republican",
	"Party ID: Independent", "Party ID: Other", "Party ID: Not Sure", "Senate Election 2010", 
	"Governor Election 2010"))
##this is table 5 in the paper.


#############
# and now do the same thing for reported voting, clustering at the county level
library(rms)
votereport0 <- ols(votereport ~ constreat, data=ccesdrops, weights=V101, x=T)
cluster_votereport0<-robcov(votereport0, ccesdrops$V278, method=c('efron'));
print(cluster_votereport0)

votereport1 <- ols(votereport ~ constreat + registered + age + V208, data=ccesdrops, weights=V101, x=T)
cluster_votereport1<-robcov(votereport1, ccesdrops$V278, method=c('efron'));
print(cluster_votereport1)

#v208 = gender, v212a=3pt party id
votereport2 <- ols(votereport ~ constreat + registered + age + V208 + V212a, data=ccesdrops, weights=V101, x=T)
cluster_votereport2<-robcov(votereport2, ccesdrops$V278, method=c('efron'));
print(cluster_votereport2)

votereport3 <- ols(votereport ~ constreat + registered + age + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops, weights=V101, x=T)
cluster_votereport3<-robcov(votereport3, ccesdrops$V278, method=c('efron'));

print(cluster_votereport3)

votereport4 <- lm(votereport ~ constreat + registered + age + I(age^2) + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops, weights=V101, x=T)
summary(votereport4)

require(stargazer)
stargazer(cluster_votereport0, cluster_votereport1, cluster_votereport3, label="CCES2010drops_clust_voting",
	 omit.stat=c("LL","ser","f", "AIC", "chi2"),
	covariate.labels=c("SC Treatment", "Registered Voter", "Gender: Female", "Age", "Party ID:Republican",
	"Party ID: Independent", "Party ID: Other", "Party ID: Not Sure", "Senate Election 2010", 
	"Governor Election 2010"))
##this is table 6 in the paper.


#placebo:
# do we see this in 2006?  (or maybe if CCES wasn't around then, 2008?)
head(ccestreat06)
ccesdrops06 <- subset(ccestreat06, !(ccestreat06$v1004 %in% drops$GEOid2))
dim(ccestreat06); dim(ccesdrops06)

contact0_06 <- lm(contact ~ constreat, data=ccesdrops06)
summary(contact0_06)

contact1_06 <- lm(contact ~ constreat + registered + v2004 + age, data=ccesdrops06, weights=as.numeric(v1001))
summary(contact1_06)
#2004 = gender; 3005=3pt party id

contact2_06 <- lm(contact ~ constreat + registered + v2004 + age + v3005, data=ccesdrops06, weights=as.numeric(v1001))
summary(contact2_06)

contact3_06 <- lm(contact ~ constreat + registered + v2004 + age + v3005 + had_senelection2006 + had_govelection2006, data=ccesdrops06, weights=as.numeric(v1001))
summary(contact3_06)

##should also look at reported voting.
head(ccestreat06)
vote06 <-  lm(vote ~ constreat + registered + v2004 + age, data=ccesdrops06, weights=as.numeric(v1001))
summary(vote06)

stargazer(contact0_06, contact1_06, contact3_06, label="CCES2006drops", omit.stat=c("LL","ser","f", "AIC", "chi2"))

##same deal: cluster by county
contact0_06 <- ols(contact ~ constreat, data=ccesdrops06, x=T)
cluster_contact0_06<-robcov(contact0_06, ccesdrops06$County, method=c('efron'));
print(cluster_contact0_06)

contact1_06 <- ols(contact ~ constreat + registered + v2004 + age, data=ccesdrops06, weights=as.numeric(v1001), x=T)
cluster_contact1_06<-robcov(contact1_06, ccesdrops06$County, method=c('efron'));
print(cluster_contact1_06)
#2004 = gender; 3005=3pt party id

contact2_06 <- ols(contact ~ constreat + registered + v2004 + age + v3005, data=ccesdrops06, weights=as.numeric(v1001), x=T)
cluster_contact2_06<-robcov(contact2_06, ccesdrops06$County, method=c('efron'));
print(cluster_contact2_06)

contact3_06 <- ols(contact ~ constreat + registered + v2004 + age + as.factor(v3005) + had_senelection2006 + had_govelection2006, data=ccesdrops06, weights=as.numeric(v1001),x=T)

cluster_contact3_06<-robcov(contact3_06, ccesdrops06$County, method=c('efron'));
print(cluster_contact3_06) #in any case, this is only going to make SE's more conservative and it's a placebo test; we get the idea.

stargazer(cluster_contact0_06, cluster_contact1_06, #cluster_contact3_06, 
	label="CCES2006drops_clust", omit.stat=c("LL","ser","f", "AIC", "chi2"), 
	covariate.labels=c("SC Treatment", "Registered Voter", "Gender: Female", "Age", "Party ID:Republican",
	"Party ID: Independent", "Party ID: Other", "Senate Election 2006", "Governor Election 2006"))
#Table 12 in appendix C

contact3 <- ols(contact ~ constreat + registered + age + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops, weights=V101, x=T)
cluster_contact3<-robcov(contact3, ccesdrops$V278, method=c('efron'));

print(cluster_contact3)
########################################################################################
## finally, do the other placebo (non-latino contact) too
## copy-pasting from step 1 above, but then using different respondents
## can run code from here without the above.
########################################################################################

#subset to other respondents
nonlatinos <- subset(cces, !(as.character(cces$V211)=="Hispanic" | as.character(cces$V290)=="Yes"))
dim(nonlatinos)
nonlatinosmall <- nonlatinos[,c("V211", "V290", "CC425a", "V277", "V278", "V302", "V207", "V212a", "V214", "V208", "V203", "V101", "CC401", "vote_gen10")] 
head(nonlatinosmall)
nonlatinosmall$contact <- 2-as.numeric(nonlatinosmall$CC425a) #1 if yes, 0 if no.
mean(nonlatinosmall$contact, na.rm=T)
nonlatinosmall$votereport <- 0
nonlatinosmall$votereport<- ifelse(as.numeric(nonlatinosmall$CC401)==5, 1, 0)
#okay, now need to merge in treated info using fips codes.
treatment <- read.csv("treatmentstatusofcounties.csv")
head(treatment)
treatment$County <- paste(treatment$county, treatment$State)
treatmentlong <- treatment
treatment<- treatment[,c("constreat", "County", "GEOid2")]

ccestreat_plac <- merge(nonlatinosmall, treatment, by.x="V278", by.y="County", all.x=T) 
dim(treatment); dim(nonlatinosmall); dim(ccestreat_plac)
head(ccestreat_plac)
ccestreat_plac[is.na(ccestreat_plac$constreat)==T,"constreat"]<- 0 #note that results don't depend on these places.
mean(ccestreat_plac$constreat)
ccestreat1<- ccestreat_plac

#okay, also merge in state election covariates
covars <- read.csv("statecovariates.csv", stringsAsFactors=F)
elections10 <- covars[,c("abbrev", "state", "had_senelection2010", "had_govelection2010")]
elections06 <- covars[,c("abbrev", "state", "had_senelection2006", "had_govelection2006")]

sum(is.na(ccestreat1$State)); sum(is.na(ccestreat1$state)); sum(is.na(ccestreat1$stateid))

ccestreat1$otherstate <- NA
for (i in 1:nrow(ccestreat1)){
	row <- ccestreat1[i,]
	state <- substr(row$V278, nchar(row$V278)-1, nchar(row$V278))
	ccestreat1$otherstate[i] <- state
}
head(ccestreat1)
sum(is.na(ccestreat1$otherstate))
ccestreat_plac <- merge(ccestreat1, elections10, by.x = "otherstate", by.y = "abbrev")
dim(ccestreat1); dim(elections10); dim(ccestreat_plac)
head(ccestreat_plac) 

ccestreat_plac$registered <- 2-as.numeric(ccestreat_plac$V203)
ccestreat_plac$age <- 2010-as.numeric(ccestreat_plac$V207)

contact0_pl <- lm(contact ~ constreat, data=ccestreat_plac, weights=V101)
summary(contact0_pl)

#but as before, restrict to the smaller dataset.

ccesdrops_plac <- subset(ccestreat_plac, !(ccestreat_plac$V278 %in% drops$County))
dim(ccestreat_plac); dim(ccesdrops_plac)
contact0.0_pl <- lm(contact ~ constreat, data=ccesdrops_plac)
summary(contact0.0_pl)
contact0_pl <- lm(contact ~ constreat, data=ccesdrops_plac, weights=V101)
summary(contact0_pl)
contact1_pl <- lm(contact ~ constreat + registered + age + V208, data=ccesdrops_plac, weights=V101)
summary(contact1_pl)
#v208 = gender, v212a=3pt party id
contact2_pl <- lm(contact ~ constreat + registered + age + V208 + V212a, data=ccesdrops_plac, weights=V101)
summary(contact2_pl)

contact3_pl <- lm(contact ~ constreat + registered + age + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops_plac, weights=V101)
summary(contact3_pl)

##now for the final version, cluster by county
library(rms)
contact0_pl <- ols(contact ~ constreat, data=ccesdrops_plac, weights=V101, x=T)
cluster_contact0_pl<-robcov(contact0_pl, ccesdrops_plac$V278, method=c('efron'));
print(cluster_contact0_pl)

contact1_pl <- ols(contact ~ constreat + registered + age + V208, data=ccesdrops_plac, weights=V101, x=T)
cluster_contact1_pl<-robcov(contact1_pl, ccesdrops_plac$V278, method=c('efron'));
print(cluster_contact1_pl)

#v208 = gender, v212a=3pt party id
contact2_pl <- ols(contact ~ constreat + registered + age + V208 + V212a, data=ccesdrops_plac, weights=V101, x=T)
cluster_contact2_pl<-robcov(contact2_pl, ccesdrops_plac$V278, method=c('efron'));
print(cluster_contact2_pl)

contact3_pl <- ols(contact ~ constreat + registered + age + V208 + V212a + had_senelection2010 + had_govelection2010, data=ccesdrops_plac, weights=V101, x=T)
cluster_contact3_pl<-robcov(contact3_pl, ccesdrops_plac$V278, method=c('efron'));

print(cluster_contact3_pl)


require(stargazer)
stargazer(cluster_contact0_pl, cluster_contact1_pl, cluster_contact3_pl, label="CCES2010dropsplacebo_clust",
	 omit.stat=c("LL","ser","f", "AIC", "chi2"),
	covariate.labels=c("SC Treatment", "Registered Voter", "Gender: Female", "Age", "Party ID:Republican",
	"Party ID: Independent", "Party ID: Other", "Party ID: Not Sure", "Senate Election 2010", 
	"Governor Election 2010"))
##this is table 13 in appendix A


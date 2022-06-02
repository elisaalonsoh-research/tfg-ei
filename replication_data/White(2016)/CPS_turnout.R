### Ariel White
### replication code for "When Threat Mobilizes"
### Plotting CPS turnout data
### spring 2016

rm(list=ls())
setwd("/home/ariel/Desktop/SCanalysis/replicationdata")

stateCPS <- read.csv("/home/ariel/Desktop/SCanalysis/enrollmentpatterns/statelevelturnoutfromCPS.csv")

treated <- subset(stateCPS, stateCPS$gestfips%in%c("DE", "FL", "VA", "TX", "WV"))
untreated <- subset(stateCPS, !(stateCPS$gestfips %in% c("DE", "FL", "VA", "TX", "WV")))
head(treated)

treatmean <- aggregate(treated$latinocitizenturnout, by=list(treated$year), FUN=mean)
untreatmean <- aggregate(untreated$latinocitizenturnout, by=list(untreated$year), FUN=mean)

odds <-c(1, 3, 5)
evens <- c(2, 4, 6)

#do a weighted mean here
head(stateCPS)

smalltreat <- treated[, c("latinocitizenturnout", "countvar", "year")]

require(plyr)
treatwmean <-ddply(smalltreat,.(year),summarise, wm = weighted.mean(latinocitizenturnout,countvar))
smalluntreat <- untreated[, c("latinocitizenturnout", "countvar", "year")]
untreatwmean <-ddply(smalluntreat,.(year),summarise, wm = weighted.mean(latinocitizenturnout,countvar))

pdf("treatmentturnout_all_greyscale.pdf")
plot(untreatwmean$year, untreatwmean$wm, col="dodgerblue4", type="b", ylim=c(0, 1), 
	#main="Latino Citizen Turnout in Treated and Untreated States, from CPS",
	ylab="Latino Citizen Voter Turnout", xlab="Year", lwd=2.5)
title(expression(phantom("Latino Citizen Turnout in ") * 
  "Treated" * phantom("and Untreated States, from CPS")),col.main="firebrick")
title(expression(phantom("Latino Citizen Turnout in Treated and ") * 
  "Untreated" * phantom(" States, from CPS")),col.main="dodgerblue4")
title(expression("Latino Citizen Turnout in " * phantom("Treated ") *
  "and " * phantom("Untreated")* " States, from CPS" ,col.main="black")) 
points(treatwmean$year, treatwmean$wm, col="firebrick", type="b", lwd=4.5)
dev.off() ##second half of Figure 4 from Appendix A

#also plot all the lines
statenames <- as.character(stateCPS$gestfips)
pdf(file="statetrendsfromCPS.pdf")
for (i in 1){
	thestate <- unique(statenames)[i]
	statedata <- subset(stateCPS, stateCPS$gestfips == thestate)
	color <- ifelse(thestate %in% c("DE", "FL", "VA", "TX", "WV"), "firebrick", "dodgerblue4")
	plot(statedata$year, statedata$latinocitizenturnout, col=color, type="b", ylim=c(0, 1), 
		ylab="Proportion of eligible Latino voters turning out", xlab="Year", main="Latino Turnout Trends in All States (from CPS)")	
}

for (i in 2:length(unique(statenames))){
	thestate <- unique(statenames)[i]
	statedata <- subset(stateCPS, stateCPS$gestfips == thestate)
	color <- ifelse(thestate %in% c("DE", "FL", "VA", "TX", "WV"), "firebrick", "dodgerblue4")
	lwd <- ifelse(thestate %in% c("DE", "FL", "VA", "TX", "WV"), 2.5, 1.25)
	points(statedata$year, statedata$latinocitizenturnout, col=color, type="b", lwd=lwd)	
}
dev.off() #first half of Figure 4 from Appendix A

#
#merge in state treatment status and make table predicting treatment with CPS ests.
state1 <- read.csv("mainstateleveldata_sept2013.csv")
statetreatment <- state1[,c("state", "abbrev", "statetreat")]
statetreatment$gestfips <- as.character(statetreatment$abbrev)
statetreatment[50,4] <- ("AZ") #AZ missing from that dataset due to early enrollment; add it back in for completeness here.
statetreatment[50,3] <- 0

state3 <- merge(stateCPS, statetreatment, by="gestfips", all.x=T)
dim(state3); dim(stateCPS);  dim(statetreatment)

#then reshape this wide.
state4 <- reshape(state3, timevar="year", idvar=c("gestfips", "state", "abbrev", "statetreat"), direction="wide")

ckall2_full <- glm(statetreat ~ latinocitizenturnout.1996 + latinocitizenturnout.1998 + latinocitizenturnout.2000 
	+ latinocitizenturnout.2002 + latinocitizenturnout.2004 + latinocitizenturnout.2006,
	data=state4, family="binomial")
summary(ckall2_full)


##run the above, but dropping places with fewer than 30 respondents per state-year
states4 <- state4[state4$countvar.1996>=30, ]
dim(states4)
stateCPS <- stateCPS[stateCPS$countvar>=30,]
dim(stateCPS)


ckall2 <- glm(statetreat ~ latinocitizenturnout.1996 + latinocitizenturnout.1998 + latinocitizenturnout.2000 
	+ latinocitizenturnout.2002 + latinocitizenturnout.2004 + latinocitizenturnout.2006,
	data=states4, family="binomial")
summary(ckall2)

library(apsrtable)
apsrtable(ckall2, ckall2_full)
stargazer(ckall2, ckall2_full, title = "Predicting treatment with prior turnout from CPS",
	label= "treatcheck", covariate.labels=c("Latino Citizen Turnout, 1996", "Latino Citizen Turnout, 1998",
	"Latino Citizen Turnout, 2000","Latino Citizen Turnout, 2002","Latino Citizen Turnout, 2004","Latino Citizen Turnout, 2006"))
##table 8 from appendix A








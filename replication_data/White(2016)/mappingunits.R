### Ariel White, Secure Communities project
### Plotting/mapping units' treatment status

rm(list=ls())
setwd("/home/ariel/Desktop/SCanalysis/replicationdata/")
require(foreign)
units <- read.csv("treatmentstatusofcounties.csv", stringsAsFactors=F)
units[1815, "county"]<- "Dona Ana" #drop accent
units$county1 <- paste(tolower(units$state),"," , tolower(units$county), sep="") #format to match
head(units)
units <- units[, !(names(units)%in% c("county"))] #drop county for later merge.
dropped <- read.csv("droppedcounties.csv", stringsAsFactors=F)
head(dropped)
dropped[154,"county"]<- "Dona Ana"
dropped$county1 <- paste(tolower(dropped$state),"," , tolower(dropped$county), sep="")
dropped <- dropped[, !(names(dropped)%in% c("county"))]
dropped$dropped <- 1

#now merge data and map.
#grabbed code from here: http://r.789695.n4.nabble.com/Coloring-counties-on-a-full-US-map-based-on-a-certain-criterion-td4292761.html

require(maps)
require(RColorBrewer)
### My criterion for all counties: 
allcounties<-data.frame(county=map('county', plot=FALSE)$names) 
head(allcounties, 10)
allcounties$key <- as.numeric(rownames(allcounties))
head(units)

library(RecordLinkage)

ClosestMatch2 = function(string, stringVector){
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}

for (i in 1:nrow(allcounties)){
	string <- as.character(allcounties$county[i])
	matcht <- ClosestMatch2(string, unlist(units$county))
	allcounties$county1[i]<- unlist(matcht)
}
head(allcounties); class(allcounties$county1)

allcounties1 <- merge(allcounties, units, by.x="county1", by.y="county1", all.x=T, incomparables=NA)
dim(allcounties); dim(allcounties1); dim(units)
#hmm: the county-equivalent cities may not merge/show up this way.  oh well for now.

head(allcounties1)
allcounties2 <- merge(allcounties1, dropped, by.x="county1", by.y="county1", all.x=T, incomparables=NA)
dim(allcounties); dim(allcounties2); dim(dropped)

head(allcounties2)
sum(allcounties2$constreat, na.rm=T) #oh pretty close actually.

allcounties2$merged <- 0
allcounties2[is.na(allcounties2$constreat)==F, "merged"] <- 1
allcounties2[is.na(allcounties2$dropped)==F, "merged"] <- 1
sum(allcounties2$merged, na.rm=T); dim(allcounties2)
#so this is likely due to some naming things, and some counties being merged together.
probs <- subset(allcounties2, allcounties2$merged==0) #fixed now.

allcounties2$treatment <- 2
allcounties2[ which(is.na(allcounties2$constreat)==F & allcounties2$constreat==1), "treatment"] <-3
allcounties2[is.na(allcounties2$dropped)==F, "treatment"] <- 1
mean(allcounties2$treatment, na.rm=T)
head(allcounties2)
dim(subset(allcounties2, allcounties2$treatment==2))

allcounties3 <- allcounties2[with(allcounties2, order(key)), ]
head(allcounties3) #reordered by original order so it plots properly

### My colors: 
display.brewer.all()
classcolors <- brewer.pal(3, "Spectral")
classcolors <- c("white", "orange", "blue")
head(allcounties2)
### My US map: 
pdf("treatmentmap.pdf", h=9, w=12)
map('county',fill=TRUE,col=classcolors[allcounties3$treatment],lty=1,bg = 
"transparent", mar = c(1, 1.1, 1, 0.1)) 
length(allcounties3$treatment)
dev.off()
### For state borders: 
map('state', lwd=1, add=TRUE) 



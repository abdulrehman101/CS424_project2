# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard) #To allow the making of UI Dashboard
library(shinyWidgets)
library(leaflet)
library(lubridate)
library(stringr)
library(plyr)
library(dplyr)
library(DT)

#For the alert about box
library(shinyalert)

# For the tabBox
library(plotly)


# reading in data, not sure if this is correct, document says some sort of preprocessing is required, search alot but came up with this
# this reads in all the data
atlantic <- read.csv(file="./atlantic.csv",sep=",",header = FALSE,stringsAsFactors = FALSE)

#can't use columns I guess because there is 3 col row for every hurricane
hurricanes <- subset(atlantic,V4=="")

atlantic$V3[atlantic$V3 == "  "] <- "0"

# i think we need to remove those info line for every hurricane to process that data, so putting that
# in the V21 column

atlantic$names <- ""

count = 0
hurricane = ""
hurrNames = ""

for(row in 1:nrow(atlantic)){
  if(count == 0){
    count = as.numeric(atlantic[row,"V3"])
    hurricane = atlantic[row,"V1"]
    hurrNames = atlantic[row,"V2"]  
  }
  else{
    atlantic[row,"V21"] <- hurricane
    atlantic[row,"names"] <- hurrNames
    count = count - 1
  }
}
rm(count)
rm(row)
rm(hurrNames)
rm(hurricane)
hurrName = atlantic$names[1]
#list of hurricanes beginning 2005

hurrNames = subset(atlantic, V4 == "")

# now we can remove those weird a** rows

atlantic <- subset(atlantic, V4 != "")

# fix the date
atlantic$V1 <- as.Date(atlantic$V1, format = "%Y%m%d")
atlantic <- subset(atlantic, year(V1) >= 2005)

# this code creates hurname05_18 that has all the hurricanes in choronological order(Date)
# you can copy paste this in your code assuming your environment has atlantic dataframe

name <- atlantic[1,"names"]
hurrnames05_18 <- atlantic[1,]
for(row in 1:nrow(atlantic)){
  if(atlantic[row,"names"] == name){
  }
  else{
    hurrnames05_18 <- rbind(hurrnames05_18,atlantic[row,])
    name <- atlantic[row,"names"]
  }
}
hurrnames05_18 <- hurrnames05_18[c("names","V1")]
names(hurrnames05_18)[names(hurrnames05_18) == "V1"] <- "Dates"
names(hurrnames05_18)[names(hurrnames05_18) == "names"] <- "Hurricanes"

# this code creates a table with all days, and maximum wind speed on that day 

start <- as.Date("01-01-05",format="%d-%m-%y")
#end <- as.Date("05-01-05",format="%d-%m-%y")
end <- as.Date("31-12-05",format="%d-%m-%y")
finalMWAtlantic <- data.frame(V1=as.Date(character()),V7=as.integer(character()))
atlanticTrim <- atlantic[c("V1","V7")]


while(start <= end){
  dateSubset <- subset(atlanticTrim,month(V1)==month(start) & day(V1)==day(start))
  rowNum <- which.max(dateSubset$V7)
  length(rowNum)
  if(length(rowNum) == 0){
    d1 <- data.frame(start,0)
    names(d1) <- c("V1","V7")
    finalMWAtlantic <- rbind(finalMWAtlantic,d1)
  }
  else{
    d1 <- data.frame(start,dateSubset[rowNum,"V7"])
    names(d1) <- c("V1","V7")
    finalMWAtlantic <- rbind(finalMWAtlantic,d1) 
  }
  start <- start + 1
}

# this code creates a table with all days, and min pressure on that day

start <- as.Date("01-01-05",format="%d-%m-%y")
end <- as.Date("31-12-05",format="%d-%m-%y")
finalMPAtlantic <- data.frame(V1=as.Date(character()),V7=as.integer(character()))

while(start <= end){
  dateSubset <- subset(atlanticTrim,month(V1)==month(start) & day(V1)==day(start))
  rowNum <- which.min(dateSubset$V7)
  length(rowNum)
  if(length(rowNum) == 0){
    d1 <- data.frame(start,0)
    names(d1) <- c("V1","V7")
    finalMPAtlantic <- rbind(finalMPAtlantic,d1)
  }
  else{
    d1 <- data.frame(start,dateSubset[rowNum,"V7"])
    names(d1) <- c("V1","V7")
    finalMPAtlantic <- rbind(finalMPAtlantic,d1) 
  }
  start <- start + 1
}

# this code create hurrnames_maxWSpeed_05_18 that has all the hurricanes with their max speed

hurrnames_maxWSpeed_05_18 <- atlantic 
hurrnames_maxWSpeed_05_18 <- aggregate(hurrnames_maxWSpeed_05_18$V7,by=list(hurrnames_maxWSpeed_05_18$names),FUN=max)
colnames(hurrnames_maxWSpeed_05_18)
# renaming columns
names(hurrnames_maxWSpeed_05_18)[names(hurrnames_maxWSpeed_05_18) == "Group.1"] <- "Hurricanes"
names(hurrnames_maxWSpeed_05_18)[names(hurrnames_maxWSpeed_05_18) == "x"] <- "MaxWindSpeed"
# sorting in DESC wrt MaxSpeed
hurrnames_maxWSpeed_05_18 <- hurrnames_maxWSpeed_05_18[order(-hurrnames_maxWSpeed_05_18$MaxWindSpeed),]

# this code create hurrnames_minPressure_05_18 that has all the hurricanes with their max speed

hurrnames_minPressure_05_18 <- atlantic 
hurrnames_minPressure_05_18 <- aggregate(hurrnames_minPressure_05_18$V8,by=list(hurrnames_minPressure_05_18$names),FUN=min)
colnames(hurrnames_minPressure_05_18)
# renaming columns
names(hurrnames_minPressure_05_18)[names(hurrnames_minPressure_05_18) == "Group.1"] <- "Hurricanes"
names(hurrnames_minPressure_05_18)[names(hurrnames_minPressure_05_18) == "x"] <- "MinPressure"
# sorting in ASC wrt MinSpeed
hurrnames_minPressure_05_18 <- hurrnames_minPressure_05_18[order(hurrnames_minPressure_05_18$MinPressure),]



#Imaad's Code
atlantic$lat <- as.numeric(substr(atlantic$V5, 2, 5));
atlantic$lon <- as.numeric(substr(atlantic$V6, 3, 6));
atlantic$lon <- as.numeric(substr(atlantic$V6, 3, 6));
atlantic$lon <- (atlantic$lon) * -1
atlantic$names <- gsub(" ", "", atlantic$names, fixed = TRUE)
#Imaad's Code: Filtering
atlantic2018<-atlantic[(substr(atlantic$V1, 1, 4) == 2018),]
distinctNames <- unique(atlantic2018$names)

namesArray <- array(length(distinctNames))



for(val in distinctNames)
{
  assign(val, atlantic2018[(atlantic2018$names == val),])
}


cl=colors()[1:500]

color <- data.frame(cl)

set.seed(42)
delete <- sample(nrow(color))
color <- color[delete,]

color <- as.character(color)
#color <- data.frame(color)

colorName = color[1]
ColorNumber = 1
atlantic$color <- "asdf"
hurrName = atlantic$names[1]
atlantic$size <- 0
for(row in 1:nrow(atlantic)){
  if((atlantic[row,"V7"] >= 0) & (atlantic[row,"V7"] < 20)){
    atlantic[row,"size"] = 2
  }
  else if((atlantic[row,"V7"] >= 20) & (atlantic[row,"V7"] < 40)){
    atlantic[row,"size"] = 3
  }
  
  else if((atlantic[row,"V7"] >= 40) & (atlantic[row,"V7"] < 60)){
    atlantic[row,"size"] = 4
  }
  else if((atlantic[row,"V7"] >= 60) & (atlantic[row,"V7"] < 80)){
    atlantic[row,"size"] = 5
  }
  
  else if((atlantic[row,"V7"] >= 80) & (atlantic[row,"V7"] < 100)){
    atlantic[row,"size"] = 6
  }
  else if((atlantic[row,"V7"] >= 100) & (atlantic[row,"V7"] < 120)){
    atlantic[row,"size"] = 7
  }
  else{
    atlantic[row,"size"] = 8
  }
}



for(row in 1:nrow(atlantic)){
  if(atlantic[row,"size"] == 2){
    atlantic[row,"color"] <- "yellow"
  }
  else if(atlantic[row,"size"] == 3){
    atlantic[row,"color"] <- "yellow"
    
  }
  else if(atlantic[row,"size"] == 4){
    atlantic[row,"color"] <- "orange"
    
  }
  else if(atlantic[row,"size"] == 5){
    atlantic[row,"color"] <- "orange"
    
  }
  else if(atlantic[row,"size"] == 6){
    atlantic[row,"color"] <- "red"
    
  }
  else if(atlantic[row,"size"] == 7){
    atlantic[row,"color"] <- "red"
    
  }
  else{
    
    atlantic[row,"color"] <- "red"
  }
}

# * FOR PACIFIC ** #
pacific <- read.csv(file="pacific.csv",sep=",",header = FALSE,stringsAsFactors = FALSE)

#can't use columns I guess because there is 3 col row for every hurricane2
hurricane2s <- subset(pacific,V4=="")

pacific$V3[pacific$V3 == "  "] <- "0"

# i think we need to remove those info line for every hurricane2 to process that data, so putting that
# in the V21 column

pacific$names <- ""

count2 = 0
hurricane2 = ""
hurrNames2 = ""

for(row in 1:nrow(pacific)){
  if(count2 == 0){
    count2 = as.numeric(pacific[row,"V3"])
    hurricane2 = pacific[row,"V1"]
    hurrNames2 = pacific[row,"V2"]  
  }
  else{
    pacific[row,"V21"] <- hurricane2
    pacific[row,"names"] <- hurrNames2
    count2 = count2 - 1
  }
}

rm(count2)
rm(row)
rm(hurrNames2)
rm(hurricane2)

# now we can remove those weird a** rows

pacific <- subset(pacific, V4 != "")

# fix the date

pacific$V1 <- as.Date(pacific$V1, format = "%Y%m%d")

# take out data for a particular year

year = list(2005,2018)
hurrYear <- subset(pacific, year(V1) == year)
# from 2005 and onwards
pacific <- subset(pacific, year(V1) >= 2005)

# this code creates hurname05_18 that has all the hurricanes in choronological order(Date)
# you can copy paste this in your code assuming your environment has pacific dataframe

name <- pacific[1,"names"]
hurrnames05_18_p <- pacific[1,]
for(row in 1:nrow(pacific)){
  if(pacific[row,"names"] == name){
  }
  else{
    hurrnames05_18_p <- rbind(hurrnames05_18_p,pacific[row,])
    name <- pacific[row,"names"]
  }
}
hurrnames05_18_p <- hurrnames05_18_p[c("names","V1")]
names(hurrnames05_18_p)[names(hurrnames05_18_p) == "V1"] <- "Dates"
names(hurrnames05_18_p)[names(hurrnames05_18_p) == "names"] <- "Hurricanes"

# this code create hurrnames_maxWSpeed_05_18_p that has all the hurricanes with their max speed

hurrnames_maxWSpeed_05_18_p <- pacific 
hurrnames_maxWSpeed_05_18_p <- aggregate(hurrnames_maxWSpeed_05_18_p$V7,by=list(hurrnames_maxWSpeed_05_18_p$names),FUN=max)
colnames(hurrnames_maxWSpeed_05_18_p)
# renaming columns
names(hurrnames_maxWSpeed_05_18_p)[names(hurrnames_maxWSpeed_05_18_p) == "Group.1"] <- "Hurricanes"
names(hurrnames_maxWSpeed_05_18_p)[names(hurrnames_maxWSpeed_05_18_p) == "x"] <- "MaxWindSpeed"
# sorting in DESC wrt MaxSpeed
hurrnames_maxWSpeed_05_18_p <- hurrnames_maxWSpeed_05_18_p[order(-hurrnames_maxWSpeed_05_18_p$MaxWindSpeed),]

# this code create hurrnames_minPressure_05_18_p that has all the hurricanes with their max speed

hurrnames_minPressure_05_18_p <- pacific 
hurrnames_minPressure_05_18_p <- aggregate(hurrnames_minPressure_05_18_p$V8,by=list(hurrnames_minPressure_05_18_p$names),FUN=min)
colnames(hurrnames_minPressure_05_18_p)
# renaming columns
names(hurrnames_minPressure_05_18_p)[names(hurrnames_minPressure_05_18_p) == "Group.1"] <- "Hurricanes"
names(hurrnames_minPressure_05_18_p)[names(hurrnames_minPressure_05_18_p) == "x"] <- "MinPressure"
# sorting in ASC wrt MinSpeed
hurrnames_minPressure_05_18_p <- hurrnames_minPressure_05_18_p[order(hurrnames_minPressure_05_18_p$MinPressure),]

###############

# this code create hurrnames_maxWSpeed_05_18 that has all the hurricanes with their max speed

pacific_maxWSpeed_05_18 <- pacific 
pacific_maxWSpeed_05_18 <- aggregate(pacific_maxWSpeed_05_18$V7,by=list(pacific_maxWSpeed_05_18$names),FUN=max)
colnames(pacific_maxWSpeed_05_18)
# renaming columns
names(pacific_maxWSpeed_05_18)[names(pacific_maxWSpeed_05_18) == "Group.1"] <- "Hurricanes"
names(pacific_maxWSpeed_05_18)[names(pacific_maxWSpeed_05_18) == "x"] <- "MaxWindSpeed"
# sorting in DESC wrt MaxSpeed
pacific_maxWSpeed_05_18 <- pacific_maxWSpeed_05_18[order(-pacific_maxWSpeed_05_18$MaxWindSpeed),]

# this code create hurrnames_minPressure_05_18 that has all the hurricanes with their max speed

pacific_minPressure_05_18 <- pacific 
pacific_minPressure_05_18 <- aggregate(pacific_minPressure_05_18$V8,by=list(pacific_minPressure_05_18$names),FUN=min)
colnames(pacific_minPressure_05_18)
# renaming columns
names(pacific_minPressure_05_18)[names(pacific_minPressure_05_18) == "Group.1"] <- "Hurricanes"
names(pacific_minPressure_05_18)[names(pacific_minPressure_05_18) == "x"] <- "MinPressure"
# sorting in ASC wrt MinSpeed
pacific_minPressure_05_18 <- pacific_minPressure_05_18[order(pacific_minPressure_05_18$MinPressure),]

###############

###############
# this code creates a table with all days, and maximum wind speed on that day 

start <- as.Date("01-01-05",format="%d-%m-%y")
#end <- as.Date("05-01-05",format="%d-%m-%y")
end <- as.Date("31-12-05",format="%d-%m-%y")
finalMWPacific <- data.frame(V1=as.Date(character()),V7=as.integer(character()))
pacificTrim <- pacific[c("V1","V7")]


while(start <= end){
  dateSubset <- subset(pacificTrim,month(V1)==month(start) & day(V1)==day(start))
  rowNum <- which.max(dateSubset$V7)
  length(rowNum)
  if(length(rowNum) == 0){
    d1 <- data.frame(start,0)
    names(d1) <- c("V1","V7")
    finalMWPacific <- rbind(finalMWPacific,d1)
  }
  else{
    d1 <- data.frame(start,dateSubset[rowNum,"V7"])
    names(d1) <- c("V1","V7")
    finalMWPacific <- rbind(finalMWPacific,d1) 
  }
  start <- start + 1
}

# this code creates a table with all days, and min pressure on that day

start <- as.Date("01-01-05",format="%d-%m-%y")
end <- as.Date("31-12-05",format="%d-%m-%y")
finalMPPacific <- data.frame(V1=as.Date(character()),V7=as.integer(character()))

while(start <= end){
  dateSubset <- subset(pacificTrim,month(V1)==month(start) & day(V1)==day(start))
  rowNum <- which.min(dateSubset$V7)
  length(rowNum)
  if(length(rowNum) == 0){
    d1 <- data.frame(start,0)
    names(d1) <- c("V1","V7")
    finalMPPacific <- rbind(finalMPPacific,d1)
  }
  else{
    d1 <- data.frame(start,dateSubset[rowNum,"V7"])
    names(d1) <- c("V1","V7")
    finalMPPacific <- rbind(finalMPPacific,d1) 
  }
  start <- start + 1
}

maxWind <- merge(finalMWAtlantic, finalMWPacific, by="V1")
minPressure <- merge(finalMPAtlantic, finalMPPacific, by="V1")

###############

#Imaad's Code
pacific$lat <- as.numeric(substr(pacific$V5, 2, 5));
pacific$lon <- as.numeric(substr(pacific$V6, 2, 6));
pacific$lon <- (pacific$lon) * -1
pacific$names <- gsub(" ", "", pacific$names, fixed = TRUE)
#Imaad's Code: Filtering
pacific2018<-pacific[(substr(pacific$V1, 1, 4) == 2018),]




# take out data of a particular day

#date = c("2008-06-01")
#hurrYear <- subset(hurrYear,V1 == date)

# list of names of hurricane2s

hurricane2List = list()
for(row in 1:nrow(hurrYear)){
  if( hurrYear[row,"names"] == '            UNNAMED'){
    hurricane2List[length(hurricane2List) + 1] = hurrYear[row,"V21"] 
  }
  else{
    hurricane2List[length(hurricane2List) + 1] = hurrYear[row,"names"]
  }
}


hurricane2List <- data.frame("names" = matrix(unlist(hurricane2List)),stringsAsFactors=FALSE)
hurricane2List <- distinct(hurricane2List)
hurricane2List$names <- trimws(hurricane2List$names)

# top 10 hurricane2s

top10Winds <- subset(pacific,year(V1) >= 2005)

cl=colors()[1:500]

color <- data.frame(cl)

set.seed(42)
delete <- sample(nrow(color))
color <- color[delete,]

color <- as.character(color)
#color <- data.frame(color)

colorName = color[1]
ColorNumber = 1
pacific$color <- "asdf"
hurrName = pacific$names[1]

for(row in 1:nrow(pacific)){
  if((pacific[row,"V7"] >= 0) & (pacific[row,"V7"] < 20)){
    pacific[row,"size"] = 2
  }
  else if((pacific[row,"V7"] >= 20) & (pacific[row,"V7"] < 40)){
    pacific[row,"size"] = 3
  }
  
  else if((pacific[row,"V7"] >= 40) & (pacific[row,"V7"] < 60)){
    pacific[row,"size"] = 4
  }
  else if((pacific[row,"V7"] >= 60) & (pacific[row,"V7"] < 80)){
    pacific[row,"size"] = 5
  }
  
  else if((pacific[row,"V7"] >= 80) & (pacific[row,"V7"] < 100)){
    pacific[row,"size"] = 6
  }
  else if((pacific[row,"V7"] >= 100) & (pacific[row,"V7"] < 120)){
    pacific[row,"size"] = 7
  }
  else{
    pacific[row,"size"] = 8
  }
}



for(row in 1:nrow(pacific)){
  if(pacific[row,"size"] == 2){
    pacific[row,"color"] <- "yellow"
  }
  else if(pacific[row,"size"] == 3){
    pacific[row,"color"] <- "yellow"
    
  }
  else if(pacific[row,"size"] == 4){
    pacific[row,"color"] <- "orange"
    
  }
  else if(pacific[row,"size"] == 5){
    pacific[row,"color"] <- "orange"
    
  }
  else if(pacific[row,"size"] == 6){
    pacific[row,"color"] <- "red"
    
  }
  else if(pacific[row,"size"] == 7){
    pacific[row,"color"] <- "red"
    
  }
  else{
    
    pacific[row,"color"] <- "red"
  }
}
# ** END OF PACIFIC * #


atlantic2018<-atlantic[(substr(atlantic$V1, 1, 4) == 2018),]

# NumHurricane Table: Number Of Hurricanes since 2005
numHurricane <- plyr::count(year(atlantic$V1))

# numTD Table:
numTD <-atlantic[(substr(atlantic$V4, 2, 3) == "TD"),]
numTD <- plyr::count(year(numTD$V1))

# numTS Table:
numTS <-atlantic[(substr(atlantic$V4, 2, 3) == "TS"),]
numTS <- plyr::count(year(numTS$V1))

# hurrCategoryOne Table:
hurrCategoryOneTable <- atlantic[((atlantic$V7) > 63),]
hurrCategoryOneTable <- hurrCategoryOneTable[((hurrCategoryOneTable$V7) < 84),]
hurrCategoryOneTable <- hurrCategoryOneTable[order(hurrCategoryOneTable$names, -hurrCategoryOneTable$V7),]
hurrCategoryOneTable <- hurrCategoryOneTable[c(hurrCategoryOneTable$names[-1] != hurrCategoryOneTable$names[-nrow(hurrCategoryOneTable)],TRUE),]
hurrCategoryOneTable <- plyr::count(year(hurrCategoryOneTable$V1))

# hurrCategoryTwo Table:
hurrCategoryTwoTable <- atlantic[((atlantic$V7) > 82),]
hurrCategoryTwoTable <- hurrCategoryTwoTable[((hurrCategoryTwoTable$V7) < 96),]
hurrCategoryTwoTable <- hurrCategoryTwoTable[order(hurrCategoryTwoTable$names, -hurrCategoryTwoTable$V7),]
hurrCategoryTwoTable <- hurrCategoryTwoTable[c(hurrCategoryTwoTable$names[-1] != hurrCategoryTwoTable$names[-nrow(hurrCategoryTwoTable)],TRUE),]
hurrCategoryTwoTable <- plyr::count(year(hurrCategoryTwoTable$V1))

# hurrCategoryThree Table:
hurrCategoryThreeTable <- atlantic[((atlantic$V7) > 95),]
hurrCategoryThreeTable <- hurrCategoryThreeTable[((hurrCategoryThreeTable$V7) < 113),]
hurrCategoryThreeTable <- hurrCategoryThreeTable[order(hurrCategoryThreeTable$names, -hurrCategoryThreeTable$V7),]
hurrCategoryThreeTable <- hurrCategoryThreeTable[c(hurrCategoryThreeTable$names[-1] != hurrCategoryThreeTable$names[-nrow(hurrCategoryThreeTable)],TRUE),]
hurrCategoryThreeTable <- plyr::count(year(hurrCategoryThreeTable$V1))

# hurrCategoryFour Table:
hurrCategoryFourTable <- atlantic[((atlantic$V7) > 112),]
hurrCategoryFourTable <- hurrCategoryFourTable[((hurrCategoryFourTable$V7) < 137),]
hurrCategoryFourTable <- hurrCategoryFourTable[order(hurrCategoryFourTable$names, -hurrCategoryFourTable$V7),]
hurrCategoryFourTable <- hurrCategoryFourTable[c(hurrCategoryFourTable$names[-1] != hurrCategoryFourTable$names[-nrow(hurrCategoryFourTable)],TRUE),]
hurrCategoryFourTable <- plyr::count(year(hurrCategoryFourTable$V1))

# hurrCategoryFive Table:
hurrCategoryFiveTable <- atlantic[((atlantic$V7) > 136),]
hurrCategoryFiveTable <- hurrCategoryFiveTable[order(hurrCategoryFiveTable$names, -hurrCategoryFiveTable$V7),]
hurrCategoryFiveTable <- hurrCategoryFiveTable[c(hurrCategoryFiveTable$names[-1] != hurrCategoryFiveTable$names[-nrow(hurrCategoryFiveTable)],TRUE),]
hurrCategoryFiveTable <- plyr::count(year(hurrCategoryFiveTable$V1))


# * PACIFIC TABLES FROM IMAAD: RED BULLET * #
pacific2018<-pacific[(substr(pacific$V1, 1, 4) == 2018),]

# numHurricane_p Table: Number Of Hurricanes since 2005
numHurricane_p <- plyr::count(year(pacific$V1))

# numTD_p Table:
numTD_p <-pacific[(substr(pacific$V4, 2, 3) == "TD"),]
numTD_p <- plyr::count(year(numTD_p$V1))

# numTS_p Table:
numTS_p <-pacific[(substr(pacific$V4, 2, 3) == "TS"),]
numTS_p <- plyr::count(year(numTS_p$V1))

# hurrCategoryOne_p Table:
hurrCategoryOne_pTable <- pacific[((pacific$V7) > 63),]
hurrCategoryOne_pTable <- hurrCategoryOne_pTable[((hurrCategoryOne_pTable$V7) < 84),]
hurrCategoryOne_pTable <- hurrCategoryOne_pTable[order(hurrCategoryOne_pTable$names, -hurrCategoryOne_pTable$V7),]
hurrCategoryOne_pTable <- hurrCategoryOne_pTable[c(hurrCategoryOne_pTable$names[-1] != hurrCategoryOne_pTable$names[-nrow(hurrCategoryOne_pTable)],TRUE),]
hurrCategoryOne_pTable <- plyr::count(year(hurrCategoryOne_pTable$V1))

# hurrCategoryTwo_p Table:
hurrCategoryTwo_pTable <- pacific[((pacific$V7) > 82),]
hurrCategoryTwo_pTable <- hurrCategoryTwo_pTable[((hurrCategoryTwo_pTable$V7) < 96),]
hurrCategoryTwo_pTable <- hurrCategoryTwo_pTable[order(hurrCategoryTwo_pTable$names, -hurrCategoryTwo_pTable$V7),]
hurrCategoryTwo_pTable <- hurrCategoryTwo_pTable[c(hurrCategoryTwo_pTable$names[-1] != hurrCategoryTwo_pTable$names[-nrow(hurrCategoryTwo_pTable)],TRUE),]
hurrCategoryTwo_pTable <- plyr::count(year(hurrCategoryTwo_pTable$V1))

# hurrCategoryThree_p Table:
hurrCategoryThree_pTable <- pacific[((pacific$V7) > 95),]
hurrCategoryThree_pTable <- hurrCategoryThree_pTable[((hurrCategoryThree_pTable$V7) < 113),]
hurrCategoryThree_pTable <- hurrCategoryThree_pTable[order(hurrCategoryThree_pTable$names, -hurrCategoryThree_pTable$V7),]
hurrCategoryThree_pTable <- hurrCategoryThree_pTable[c(hurrCategoryThree_pTable$names[-1] != hurrCategoryThree_pTable$names[-nrow(hurrCategoryThree_pTable)],TRUE),]
hurrCategoryThree_pTable <- plyr::count(year(hurrCategoryThree_pTable$V1))

# hurrCategoryFour_p Table:
hurrCategoryFour_pTable <- pacific[((pacific$V7) > 112),]
hurrCategoryFour_pTable <- hurrCategoryFour_pTable[((hurrCategoryFour_pTable$V7) < 137),]
hurrCategoryFour_pTable <- hurrCategoryFour_pTable[order(hurrCategoryFour_pTable$names, -hurrCategoryFour_pTable$V7),]
hurrCategoryFour_pTable <- hurrCategoryFour_pTable[c(hurrCategoryFour_pTable$names[-1] != hurrCategoryFour_pTable$names[-nrow(hurrCategoryFour_pTable)],TRUE),]
hurrCategoryFour_pTable <- plyr::count(year(hurrCategoryFour_pTable$V1))

# hurrCategoryFive_p Table:
hurrCategoryFive_pTable <- pacific[((pacific$V7) > 136),]
hurrCategoryFive_pTable <- hurrCategoryFive_pTable[order(hurrCategoryFive_pTable$names, -hurrCategoryFive_pTable$V7),]
hurrCategoryFive_pTable <- hurrCategoryFive_pTable[c(hurrCategoryFive_pTable$names[-1] != hurrCategoryFive_pTable$names[-nrow(hurrCategoryFive_pTable)],TRUE),]
hurrCategoryFive_pTable <- plyr::count(year(hurrCategoryFive_pTable$V1))









ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "Project 2"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Atlantic Widgets", icon = icon("th")),
      
      prettySwitch(inputId = "atlanticSwitch", label = "Atlantic Landing", fill = TRUE, 
                   status = "danger",
                   
      ),
      
      pickerInput(
        inputId = "yearSelect",
        label = "Select/Deselect the Years 05-18:",
        choices = c("2005","2006","2007","2008","2010","2011","2012","2013","2014","2015","2016","2017","2018"), #insert the choices here
        options = list(
          'actions-box' = TRUE,
          size = 10,
          'selected-text-format' = 'count > 3'
          
        ),#End of List
        multiple = TRUE
      ),#End of pickerInput for yearSelection
      
      #Choosing a specific day to look at hurricanes, POSSIBLY CHANGE FORMAT
      #Min and max values correspond to the range and value is the starting date
      dateInput(inputId = "dateSelect", label = "Date:", min = "1851-06-25",max = "2018-11-14", value = "2018-11-14", format = "mm/dd/yy"),
      
      
      pickerInput(
        inputId = "hurrSelect",
        label = "Select/Deselect the Hurricane:",
        choices = c("Pick a year first"), #insert the choices array here
        options = list(
          'actions-box' = TRUE,
          size = 10,
          'selected-text-format' = "count > 3"
          
        ),#End of List
        multiple = TRUE
      ), #End of pickerInput for HurricaneSelection
      
      sliderInput("atlWindSpeed", label = h5("Atlantic Wind Speed"), min = 0, 
                  max = 190, value = c(0, 190),
      ),
      sliderInput("atlPressure", label = h5("Atlantic Pressure"), min = 850, 
                  max = 1050, value = c(0, 1050)
      ),      
      
      
      
      menuItem("Pacific Widgets", icon = icon("th")),
      
      prettySwitch(inputId = "pacificSwitch", label = "Pacific Landing", fill = TRUE, 
                   status = "danger",
                   
      ),
      
      pickerInput(
        inputId = "pacificYearSelect",
        label = "Select/Deselect the Years 05-18:",
        choices = c("2005","2006","2007","2008","2010","2011","2012","2013","2014","2015","2016","2017","2018"), #insert the choices here
        options = list(
          'actions-box' = TRUE,
          size = 10,
          'selected-text-format' = 'count > 3'
          
        ),#End of List
        multiple = TRUE
      ),#End of pickerInput for yearSelection
      
      
      #Choosing a specific day to look at hurricanes, POSSIBLY CHANGE FORMAT
      #Min and max values correspond to the range and value is the starting date
      dateInput(inputId = "datePacificSelect", label = "Date:", min = "1851-06-25",max = "2018-11-14", value = "2018-11-14", format = "mm/dd/yy"),
      
      
      pickerInput(
        inputId = "pacificHurrSelect",
        label = "Select/Deselect the Hurricane:",
        choices = c("Pick a year first"), #insert the choices array here
        options = list(
          'actions-box' = TRUE,
          size = 10,
          'selected-text-format' = "count > 3"
          
        ),#End of List
        multiple = TRUE
      ),   #End of pickerInput for HurricaneSelection
      
      sliderInput("pacWindSpeed", label = h5("Pacific Wind Speed"), min = 0, 
                  max = 190, value = c(0, 190),
      ),
      sliderInput("pacPressure", label = h5("Pacific Pressure"), min = 850, 
                  max = 1050, value = c(0, 1050)
      ),
      
      actionButton("aboutPage", "About"
      )
      
      
      
    )#End of sidebarMenu body
    
  ),#end of dashboardSideBar body
  
  
  
  
  dashboardBody(
    fluidRow(
      tabBox(
        title = "Atlantic",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "atlanTabset", height = "250px", width = 4,
        tabPanel("All", plotlyOutput("atlanticAll")),
        tabPanel("Tropical Storm", plotlyOutput("atlStormPlot")),
        tabPanel("Tropical Depression", plotlyOutput("atlDepressPlot"))
      ),
      
      tabBox(
        title = "Atlantic Categories",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "atlanticCategoryTabset", height = "250px", width = 4,
        tabPanel("C1", plotlyOutput("atlanticC1")),
        tabPanel("C2", plotlyOutput("atlanticC2")),
        tabPanel("C3", plotlyOutput("atlanticC3")),
        tabPanel("C4", plotlyOutput("atlanticC4")),
        tabPanel("C5", plotlyOutput("atlanticC5"))
      ),
      box(
        title = "Atlantic Map", status = "warning", solidHeader = TRUE,
        leafletOutput("atlanticMap"), #change to atlanticMap
        width = 4,
      ),
      
    ), # End of fluidRow 1
    
    fluidRow(
      tabBox(
        title = "Pacific",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "pacificTabset", height = "250px", width = 4,
        tabPanel("All", plotlyOutput("pacificAll")),
        tabPanel("Tropical Storm", plotlyOutput("pacStormPlot")),
        tabPanel("Tropical Depression", plotlyOutput("pacDepressPlot"))
      ),
      tabBox(
        title = "Pacific Categories",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "pacificCategoryTabset", height = "250px", width = 4,
        tabPanel("C1", plotlyOutput("pacificC1")),
        tabPanel("C2", plotlyOutput("pacificC2")),
        tabPanel("C3", plotlyOutput("pacificC3")),
        tabPanel("C4", plotlyOutput("pacificC4")),
        tabPanel("C5", plotlyOutput("pacificC5"))
      ),
      
      
      box(
        title = "Pacific Map", status = "warning", solidHeader = TRUE,
        leafletOutput("pacificMap"), #change to pacificMap
        width = 4,
      ),
      
    ), # End of fluidRow 2
    fluidRow(
      tabBox(
        id = 'dataset',
        tabPanel("Chronological Order", DT::dataTableOutput("table1")),
        tabPanel("Max wind speed", DT::dataTableOutput("table2")),
        tabPanel("Minimum Pressure", DT::dataTableOutput("table3"))),
      tabBox(
        id = 'dataset2',
        tabPanel("Chronological Order", DT::dataTableOutput("table1_p")),
        tabPanel("Max wind speed", DT::dataTableOutput("table2_p")),
        tabPanel("Minimum Pressure", DT::dataTableOutput("table3_p")))
    ),
    fluidRow(
      plotOutput("plot1")
    ),
    fluidRow(
      plotOutput("plot2")
    ),
  ),
  
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  observeEvent(input$aboutPage, {
    showNotification("Authors: Imaad, Abdul, Jaoudat 
                      This data originated from the 
                      Atlantic hurricane database (HURDAT2) 1851-2018 and the Northeast and North Central 
                      Pacific hurricane database (HURDAT2) 1949-2018  at http://www.nhc.noaa.gov/data/#hurdat
                      
                      Libraries used: shiny,shinydashboard, shinyWidgets, leaflet,
                                      lubridate, stringr, plyr,dplyr,DT
                     
                     ")
  })
  
  
  
  observeEvent(input$dateSelect,{
    atlantic2 <- atlantic[order(atlantic$names),]
    
    atlantic2<-atlantic2[((atlantic2$V1) == input$dateSelect),]
    
    atlantic2 <- atlantic2[c(atlantic2$names[-1] != atlantic2$names[-nrow(atlantic2)],TRUE),]
    
    allLandingNames <- atlantic2$names
    
    atlantic2 <- atlantic[Reduce(`|`, lapply(as.data.frame(atlantic), function(x) x %in% allLandingNames)),]
    
    atlanticLanding2018<-atlantic2[(substr(atlantic2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlanticLanding2018$lat, lng = atlanticLanding2018$lon, radius = atlanticLanding2018$size,color = atlanticLanding2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlanticLanding2018[atlanticLanding2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
    
  })
  
  
  observeEvent(input$datePacificSelect,{
    pacific2 <- pacific[order(pacific$names),]
    
    pacific2<-pacific2[((pacific2$V1) == input$datePacificSelect),]
    
    pacific2 <- pacific2[c(pacific2$names[-1] != pacific2$names[-nrow(pacific2)],TRUE),]
    
    allLandingNames <- pacific2$names
    
    pacific2 <- pacific[Reduce(`|`, lapply(as.data.frame(pacific), function(x) x %in% allLandingNames)),]
    
    pacificLanding2018<-pacific2[(substr(pacific2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$pacificMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = pacificLanding2018$lat, lng = pacificLanding2018$lon, radius = pacificLanding2018$size,color = pacificLanding2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = pacific2018[pacific2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = pacific2018$names)
        }
        map
      }
      
    })
    
  })
  
  observeEvent(input$atlanticSwitch,{
    
    atlantic2 <- atlantic[order(atlantic$names),]
    atlantic2<-atlantic2[((atlantic2$V3) == " L"),]
    atlantic2 <- atlantic2[c(atlantic2$names[-1] != atlantic2$names[-nrow(atlantic2)],TRUE),]
    
    allLandingNames <- atlantic2$names
    
    atlantic2 <- atlantic[Reduce(`|`, lapply(as.data.frame(atlantic), function(x) x %in% allLandingNames)),]
    
    atlanticLanding2018<-atlantic2[(substr(atlantic2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
    
    
  })
  
  observeEvent(input$pacificSwitch,{
    
    pacific2 <- pacific[order(pacific$names),]
    pacific2<-pacific2[((pacific2$V3) == " L"),]
    pacific2 <- pacific2[c(pacific2$names[-1] != pacific2$names[-nrow(pacific2)],TRUE),]
    
    allLandingNames <- pacific2$names
    
    pacific2 <- pacific[Reduce(`|`, lapply(as.data.frame(pacific), function(x) x %in% allLandingNames)),]
    
    pacificLanding2018<-pacific2[(substr(pacific2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$pacificMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = pacific2018$lat, lng = pacific2018$lon, radius = pacific2018$size,color = pacific2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = pacific2018[pacific2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = pacific2018$names)
        }
        map
      }
      
    })
    
    
  })
  
  
  observeEvent(input$atlPressure,{
    
    atlantic2 <- atlantic[order(atlantic$names, atlantic$V8),]
    atlantic2 <- atlantic2[c(atlantic2$names[-1] != atlantic2$names[-nrow(atlantic2)],TRUE),]
    
    
    atlantic2<-atlantic2[((atlantic2$V8) >= input$atlPressure[1]),]
    atlantic2<-atlantic2[((atlantic2$V8) <= input$atlPressure[2]),]
    
    maxWindSpeedNamesList <- atlantic2$names
    
    atlantic2 <- atlantic[Reduce(`|`, lapply(as.data.frame(atlantic), function(x) x %in% maxWindSpeedNamesList)),]
    
    atlantic2018<-atlantic2[(substr(atlantic2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
    
    
  })
  
  
  
  observeEvent(input$atlWindSpeed,{
    
    atlantic2 <- atlantic[order(atlantic$names, atlantic$V7),]
    atlantic2 <- atlantic2[c(atlantic2$names[-1] != atlantic2$names[-nrow(atlantic2)],TRUE),]
    
    
    atlantic2<-atlantic2[((atlantic2$V7) >= input$atlWindSpeed[1]),]
    atlantic2<-atlantic2[((atlantic2$V7) <= input$atlWindSpeed[2]),]
    
    maxWindSpeedNamesList <- atlantic2$names
    
    atlantic2 <- atlantic[Reduce(`|`, lapply(as.data.frame(atlantic), function(x) x %in% maxWindSpeedNamesList)),]
    
    atlantic2018<-atlantic2[(substr(atlantic2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
    
    
  })
  
  observeEvent(input$pacPressure,{
    
    pacific2 <- pacific[order(pacific$names, pacific$V8),]
    pacific2 <- pacific2[c(pacific2$names[-1] != pacific2$names[-nrow(pacific2)],TRUE),]
    
    
    pacific2<-pacific2[((pacific2$V8) >= input$pacPressure[1]),]
    pacific2<-pacific2[((pacific2$V8) <= input$pacPressure[2]),]
    
    maxWindSpeedNamesList <- pacific2$names
    
    pacific2 <- pacific[Reduce(`|`, lapply(as.data.frame(pacific), function(x) x %in% maxWindSpeedNamesList)),]
    
    pacific2018<-pacific2[(substr(pacific2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$pacificMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = pacific2018$lat, lng = pacific2018$lon, radius = pacific2018$size,color = pacific2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = pacific2018[pacific2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = pacific2018$names)
        }
        map
      }
      
    })
    
    
  })
  
  
  
  observeEvent(input$pacWindSpeed,{
    
    pacific2 <- pacific[order(pacific$names, pacific$V7),]
    pacific2 <- pacific2[c(pacific2$names[-1] != pacific2$names[-nrow(pacific2)],TRUE),]
    
    
    pacific2<-pacific2[((pacific2$V7) >= input$pacWindSpeed[1]),]
    pacific2<-pacific2[((pacific2$V7) <= input$pacWindSpeed[2]),]
    
    maxWindSpeedNamesList <- pacific2$names
    
    pacific2 <- pacific[Reduce(`|`, lapply(as.data.frame(pacific), function(x) x %in% maxWindSpeedNamesList)),]
    
    pacific2018<-pacific2[(substr(pacific2$V1, 1, 4) == 2018),]
    
    
    
    
    
    output$pacificMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = pacific2018$lat, lng = pacific2018$lon, radius = pacific2018$size,color = pacific2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = pacific2018[pacific2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = pacific2018$names)
        }
        map
      }
      
    })
    
    
  })
  
  output$pacificMap <- renderLeaflet({
    {
      
      map <- leaflet() %>% 
        addTiles(group = "Normal") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
        addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
        addCircleMarkers(lat = pacific2018$lat, lng = pacific2018$lon, radius = pacific2018$size,color = pacific2018$color) %>%
        
        
        
        addLayersControl(
          baseGroups = c("Normal", "Earth", "Ocean"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      for(val in distinctNames)
      {
        map <- addPolylines(map, data = pacific2018[pacific2018$names == val,], 
                            lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = pacific2018$names)
      }
      map
    }
    
  })
  
  output$atlanticMap <- renderLeaflet({
    {
      
      map <- leaflet() %>% 
        addTiles(group = "Normal") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
        addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
        addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
        
        
        
        addLayersControl(
          baseGroups = c("Normal", "Earth", "Ocean"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      for(val in distinctNames)
      {
        map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                            lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
      }
      map
    }
    
  })
  
  observeEvent(input$yearSelect,{
    #To update the hurricane dropdown box to contain hurricane names of that specifc year
    year = input$yearSelect
    
    hurrYear <- subset(atlantic, year(V1) == year)
    
    hurricaneList = list()
    for(row in 1:nrow(hurrYear)){
      if( hurrYear[row,"names"] == '            UNNAMED'){
        hurricaneList[length(hurricaneList) + 1] = hurrYear[row,"V21"] 
      }
      else{
        hurricaneList[length(hurricaneList) + 1] = hurrYear[row,"names"]
      }
    }
    
    hurricaneList <- data.frame("names" = matrix(unlist(hurricaneList)),stringsAsFactors=FALSE)
    hurricaneList <- distinct(hurricaneList)
    hurricaneList$names <- trimws(hurricaneList$names)
    
    
    #To update the hurricane names in the drop down boxes
    updatePickerInput(session = session, inputId = "hurrSelect", choices = hurricaneList) #Change the choices to update 
    
    
    atlantic2018<-atlantic[(substr(atlantic$V1, 1, 4) == input$yearSelect),]
    
    # update map depending on the year
    atlantic2018<-atlantic[(substr(atlantic$V1, 1, 4) == input$yearSelect),]
    
    # show new map
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
  }
  
  
  
  )#End of yearSelect observeEvent
  
  observeEvent(input$hurrSelect,{
    
    # update map depending on the year
    atlantic2018<-atlantic[((atlantic$names) == input$hurrSelect),]
    
    # show new map
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
  })
  
  output$table1 <- renderDataTable({
    datatable(hurrnames05_18)
  })
  output$table2 <- renderDataTable({
    datatable(hurrnames_maxWSpeed_05_18)
  })
  output$table3 <- renderDataTable({
    datatable(hurrnames_minPressure_05_18)
  })
  
  output$table1_p <- renderDataTable({
    datatable(hurrnames05_18_p)
  })
  output$table2_p <- renderDataTable({
    datatable(hurrnames_maxWSpeed_05_18_p)
  })
  output$table3_p <- renderDataTable({
    datatable(hurrnames_minPressure_05_18_p)
  })
  
  
  observeEvent(input$yearSelect,{
    #To update the hurricane dropdown box to contain hurricane names of that specifc year
    year = input$yearSelect
    
    hurrYear <- subset(atlantic, year(V1) == year)
    
    hurricaneList = list()
    for(row in 1:nrow(hurrYear)){
      if( hurrYear[row,"names"] == '            UNNAMED'){
        hurricaneList[length(hurricaneList) + 1] = hurrYear[row,"V21"] 
      }
      else{
        hurricaneList[length(hurricaneList) + 1] = hurrYear[row,"names"]
      }
    }
    
    hurricaneList <- data.frame("names" = matrix(unlist(hurricaneList)),stringsAsFactors=FALSE)
    hurricaneList <- distinct(hurricaneList)
    hurricaneList$names <- trimws(hurricaneList$names)
    
    
    #To update the hurricane names in the drop down boxes
    updatePickerInput(session = session, inputId = "hurrSelect", choices = hurricaneList) #Change the choices to update 
    
    
    atlantic2018<-atlantic[(substr(atlantic$V1, 1, 4) == input$yearSelect),]
    
    # update map depending on the year
    atlantic2018<-atlantic[(substr(atlantic$V1, 1, 4) == input$yearSelect),]
    
    # show new map
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
  }
  
  
  )#End of yearSelect observeEvent
  
  observeEvent(input$PacificYearSelect,{
    #To update the hurricane dropdown box to contain hurricane names of that specifc year
    year = input$PacificYearSelect
    
    hurrYear <- subset(pacific, year(V1) == year)
    
    hurricaneList = list()
    for(row in 1:nrow(hurrYear)){
      if( hurrYear[row,"names"] == '            UNNAMED'){
        hurricaneList[length(hurricaneList) + 1] = hurrYear[row,"V21"] 
      }
      else{
        hurricaneList[length(hurricaneList) + 1] = hurrYear[row,"names"]
      }
    }
    
    hurricaneList <- data.frame("names" = matrix(unlist(hurricaneList)),stringsAsFactors=FALSE)
    hurricaneList <- distinct(hurricaneList)
    hurricaneList$names <- trimws(hurricaneList$names)
    
    
    #To update the hurricane names in the drop down boxes
    updatePickerInput(session = session, inputId = "pacificHurrSelect", choices = hurricaneList) #Change the choices to update 
    
    
    pacific2018<-pacific[(substr(pacific$V1, 1, 4) == input$PacificYearSelect),]
    
    # update map depending on the year
    pacific2018<-pacific[(substr(pacific$V1, 1, 4) == input$PacificYearSelect),]
    
    # show new map
    output$pacificMap <- renderLeaflet({
      
      
      map <- leaflet() %>% 
        addTiles(group = "Normal") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
        addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
        addCircleMarkers(lat = pacific2018$lat, lng = pacific2018$lon, radius = pacific2018$size,color = pacific2018$color) %>%
        
        
        
        addLayersControl(
          baseGroups = c("Normal", "Earth", "Ocean"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      for(val in distinctNames)
      {
        map <- addPolylines(map, data = pacific2018[pacific2018$names == val,], 
                            lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = pacific2018$names)
      }
      map
      
      
    })
  })
  
  observeEvent(input$hurrSelect,{
    
    # update map depending on the year
    atlantic2018<-atlantic[((atlantic$names) == input$hurrSelect),]
    
    # show new map
    output$atlanticMap <- renderLeaflet({
      {
        
        map <- leaflet() %>% 
          addTiles(group = "Normal") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Earth") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
          addCircleMarkers(lat = atlantic2018$lat, lng = atlantic2018$lon, radius = atlantic2018$size,color = atlantic2018$color) %>%
          
          
          
          addLayersControl(
            baseGroups = c("Normal", "Earth", "Ocean"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        for(val in distinctNames)
        {
          map <- addPolylines(map, data = atlantic2018[atlantic2018$names == val,], 
                              lat = ~lat, lng = ~lon, group = ~names, color = "white", weight = .9, popup = atlantic2018$names)
        }
        map
      }
      
    })
  })
  
  
  # For the Different types of storms
  
  #Insert tabBox output for atlanticAll
  output$atlanticAll <- renderPlotly({
    ggplot(data=numHurricane, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
  })
  
  # Insert tabBox output for plot Atlantic Storm = atlStormPlot
  output$atlStormPlot <- renderPlotly({
    ggplot(data=numTS, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
  })
  #Insert tabBox output for Depression = atlDepressPlot
  output$atlDepressPlot <- renderPlotly({
    ggplot(data=numTD, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
  })
  
  
  # Categories for the Atlantic
  output$atlanticC1 <- renderPlotly({
    ggplot(data=hurrCategoryOneTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  
  output$atlanticC2 <- renderPlotly({
    ggplot(data=hurrCategoryTwoTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  
  output$atlanticC3 <- renderPlotly({
    ggplot(data=hurrCategoryThreeTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  output$atlanticC4 <- renderPlotly({
    ggplot(data=hurrCategoryFourTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  
  output$atlanticC5 <- renderPlotly({
    ggplot(data=hurrCategoryFiveTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  ####
  output$plot1 <- renderPlot({
    ggplot() +
      geom_line(data = maxWind, aes(x = V1, y = V7.x), color = "black")+ 
      geom_line(data = maxWind, aes(x = V1, y = V7.y), color = "blue")+
      xlab('Date')+
      ylab('Wind Speed')
  })
  output$plot2 <- renderPlot({
    ggplot() +
      geom_line(data = minPressure, aes(x = V1, y = V7.x), color = "black")+ 
      geom_line(data = minPressure, aes(x = V1, y = V7.y), color = "blue")+
      xlab('Date')+
      ylab('Minimum Pressure')
  })
  ####
  
  #Insert tabBox output for pacificAll
  output$pacificAll <- renderPlotly({
    ggplot(data=numHurricane_p, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
  })
  
  # Insert tabBox output for plot Atlantic Storm = pacStormPlot
  output$pacStormPlot <- renderPlotly({
    ggplot(data=numTS_p, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
  })
  #Insert tabBox output for Depression = pacDepressPlot
  output$pacDepressPlot <- renderPlotly({
    ggplot(data=numTD_p, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
  })
  
  #Categories for pacific
  output$pacificC1 <- renderPlotly({
    ggplot(data=hurrCategoryOne_pTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  output$pacificC2 <- renderPlotly({
    ggplot(data=hurrCategoryTwo_pTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  output$pacificC3 <- renderPlotly({
    ggplot(data=hurrCategoryThree_pTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  output$pacificC4 <- renderPlotly({
    ggplot(data=hurrCategoryFour_pTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  output$pacificC5 <- renderPlotly({
    ggplot(data=hurrCategoryFive_pTable, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("# Of Hurricanes") +  geom_bar(stat="identity", fill = "#FF6666")  
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
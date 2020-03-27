library(lubridate)
library(stringr)
library(dplyr)
# reading in data, not sure if this is correct, document says some sort of preprocessing is required, search alot but came up with this
# this reads in all the data
atlantic <- read.csv(file="atlantic.csv",sep=",",header = FALSE,stringsAsFactors = FALSE)

#can't use columns I guess because there is 3 col row for every hurricane
hurricanes <- subset(atlantic,V4=="")

atlantic$V3[atlantic$V3 == "  "] <- "0"

# i think we need to remove those info line for every hurricane to process that data, so putting that
# in the V21 column

atlantic$names <- ""

count = 0
hurricane = ""
hurrNames = ""

for(row in 53183:nrow(atlantic)){
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

# now we can remove those weird a** rows

atlantic <- subset(atlantic, V4 != "")

# fix the date

atlantic$V1 <- as.Date(atlantic$V1, format = "%Y%m%d")

# take out data for a particular year

year = list(2005,2018)
hurrYear <- subset(atlantic, year(V1) == year)
# from 2005 and onwards
atlantic <- subset(atlantic, year(V1) >= 2005)

# take out data of a particular day

#date = c("2008-06-01")
#hurrYear <- subset(hurrYear,V1 == date)

# list of names of hurricanes

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

# top 10 hurricanes

top10Winds <- subset(atlantic,year(V1) >= 2005)

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
for(row in 1:nrow(atlantic)){
  if(atlantic[row,"names"] == hurrName){
    atlantic[row,"color"] <- colorName
  }
  else{
    hurrName <- atlantic[row,"names"]
    ColorNumber = ColorNumber + 1
    colorName = color[ColorNumber]
    atlantic[row,"color"] <- colorName
  }
}

atlantic$size <- 0
for(row in 1:nrow(atlantic)){
  if((atlantic[row,"V7"] >= 0) & (atlantic[row,"V7"] < 40)){
    atlantic[row,"size"] = 2
  }
  else if((atlantic[row,"V7"] >= 40) & (atlantic[row,"V7"] < 80)){
    atlantic[row,"size"] = 4
  }
  else if((atlantic[row,"V7"] >= 80) & (atlantic[row,"V7"] < 120)){
    atlantic[row,"size"] = 6
  }
  else{
    atlantic[row,"size"] = 8
  }
}


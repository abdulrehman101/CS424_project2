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

# now we can remove those weird a** rows

atlantic <- subset(atlantic, V4 != "")

# fix the date

atlantic$V1 <- as.Date(atlantic$V1, format = "%Y%m%d")

# take out data for a particular year

year = list(2018)
hurrYear <- subset(atlantic, year(V1) == year)

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

hurrYear[1,'names']














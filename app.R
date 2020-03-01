#
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


ui <- dashboardPage(
    
    dashboardHeader(title = "Project 2"),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            pickerInput(
                inputId = "yearSelect",
                label = "Select/Deselect the Years 05-18",
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
                label = "Select/Deselect the Hurricane",
                choices = c("Morning","Afternoon","Evening","Night","default"), #insert the choices array here
                options = list(
                    'actions-box' = TRUE,
                    size = 10,
                    'selected-text-format' = "count > 3"
                    
                ),#End of List
                multiple = TRUE
            )#End of pickerInput for HurricaneSelection
            
            
            
            
            
            
                
        )#End of sidebarMenu body
        
        
        
        
    ),#end of dashboardSideBar body
    
    dashboardBody(
        fluidRow(
            
                   box(title = "About : Total", solidHeader = TRUE, status = "primary", height = 500,
                       textOutput("var")
                   ) 
            
        ),
        
    ),
    
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  
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
    
    }
               
  )#End of yearSelect observeEvent

    

  
  
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

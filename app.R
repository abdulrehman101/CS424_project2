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

ui <- dashboardPage(
    
    dashboardHeader(title = "Project 2"),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            pickerInput(
                inputId = "yearSelect",
                label = "Select/Deselect the Years 05-18",
                choices = c("Morning ","Afternoon ","Evening ","Night ","default "), #insert the choices array here
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

    #To update the hurricane names in the drop down boxes
    updatePickerInput(session = session, inputId = "hurrSelect", choices = "Update") #Change the choices to update 
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

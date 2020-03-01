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

ui <- fluidPage(
    
    dashboardHeader(title = "Project 2"),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            pickerInput(
                inputId = "yearSelect",
                label = "Select/Deselect the Years 05-18",
                choices = "2005", #insert the choices array here
                options = list(
                    'actions-box' = TRUE,
                    size = 10,
                    'selected-text-format' = "count > 3"
                    
                ),#End of List
                multiple = TRUE
            ),#End of pickerInput for yearSelection
            
            
            pickerInput(
                inputId = "hurrSelect",
                label = "Select/Deselect the Hurricane",
                choices = "Abdul", #insert the choices array here
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
        
        
    ),
    
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

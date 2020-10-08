library(shinydashboard)
library(shiny)
ui <- dashboardPage(
    dashboardHeader(title = "My Dashboard Frame"),
    dashboardSidebar(),
    dashboardBody(
        
    )
)
server <- function(input, output) {
    
}
shinyApp(ui, server)
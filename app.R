library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2) 
library(lubridate)
library(grid)

library(nycflights13)


ui <- dashboardPage(
    
    #1.header
    dashboardHeader(title = "Basic Dashboard"),
    
    #2.sidebar
    dashboardSidebar(
        menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
        menuItem("Weather", icon = icon("cloud-sun-rain"), tabName = "Weather"),
        menuItem("Airports", icon = icon("synagogue"), tabName = "Airports"),
        menuItem("Airlines", icon = icon("warehouse"), tabName = "Airlines"),
        menuItem("Flights", icon = icon("plane"), tabName = "Flights")
    ),
    
    #3.body
    dashboardBody(
        tabItems(
            
            ##3.1 dashboard
            tabItem( 
                
                tabName = "dashboard",
                helpText("Throughout this dashboard, we’re going to analyze data related 
               to all domestic flights departing from one of New York City’s three 
               main airports in 2013. We’ll access this data using the nycflights13 R package"),
                
                br(),
                br(),
                
                dateInput("date",                       
                          h4("Date"),
                          value = "2013-01-01"),
                
                
                
                fluidRow(
                    valueBoxOutput("flightsBox"),
                    valueBoxOutput("ontimeBox"),
                    valueBoxOutput("delayBox")
                )
            ),
            
            
            ##3.2 Weather
            tabItem(
                tabName = "Weather",
                helpText("Hourly meteorological data for each of the three NYC airports. 
               This data frame has 26,115 rows.")
            ),
            
            ##3.3 Airports
            tabItem(
                tabName = "Airports",
                helpText(" Newark Liberty International (EWR), 
                  John F. Kennedy International (JFK), 
                  and LaGuardia Airport (LGA).")
            ),
            
            
            ##3.4 Airlines
            tabItem(
                tabName = "Airlines",
                helpText("A table matching airline names and their two-letter International Air Transport Association (IATA) airline codes (also known as carrier codes) for 16 airline companies. 
                  For example, “DL” is the two-letter code for Delta.")
            ),
            
            
            ##3.5 Flights
            tabItem(
                tabName = "Flights",
                helpText("Information on all 336,776 flights."),
                
                br(),
                br(),
                
                dateInput("date_35",                       
                          h4("Departure Date"),
                          value = "2013-01-01"),
                
                sliderInput("month_35", label = h4("Departure Month"), min = 1, 
                            max = 12, value = c(1,6)),
            
                
                plotOutput("line_35")
                
                
                
                
            )
            
        )
    )
)

server <- function(input, output) {
    
    output$line_35 <- renderPlot({ 
        
        day_flights <- flights %>%
            filter(month == month(input$date_35), day == day(input$date_35)) %>%
            group_by(month, day, hour, origin) %>%
            summarise(count = n()) 
        
        ggplot(day_flights, aes(x=hour, y=count, group=origin)) +
            geom_line(aes(linetype = origin))
    })
    
    ggplot(day_flights, aes(x=hour, y=count, group=origin)) +
        geom_line(aes(linetype = origin))
}
    

shinyApp(ui, server)
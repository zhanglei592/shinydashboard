library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2) 
library(lubridate)
library(grid)

library(nycflights13)

delay_flights <- flights %>%
    filter(arr_delay > 0)  %>%
    summarise(count = n(),
              delay_med_min = median(arr_delay,na.rm = FALSE),
              delay_avg_min = mean(arr_delay,na.rm = FALSE)
    )

all_flights <- flights %>%
    summarise(all_c = n())   %>% 
    mutate(delay_per = round((1-delay_flights$count / all_c)*100,1),
           round(delay_flights["delay_med_min"],digits =1),
           round(delay_flights["delay_avg_min"],digits =1),
           delay_flights= delay_flights$count,
           on_time = length(which(flights["arr_delay"]<=0)),
           small_delay = length(which(flights["arr_delay"]>0 & flights["arr_delay"]<=15)),
           medium_delay = length(which(flights["arr_delay"]>15 & flights["arr_delay"]<=45)),
           large_delay = length(which(flights["arr_delay"]>45 & flights["arr_delay"]<=180)),
           huge_delay = length(which(flights["arr_delay"]>180))
    )
 

ui <- dashboardPage(
    
    #1.header-------------------------------------------------------------------
    #add nav bar
    dashboardHeader(title = "Basic Dashboard"),
    
    #2.sidebar------------------------------------------------------------------
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
            
            ##3.1 dashboard------------------------------------------------------
            
            tabItem( 
                
                tabName = "dashboard",
                helpText("Throughout this dashboard, we’re going to analyze data related 
               to all domestic flights departing from one of New York City’s three 
               main airports in 2013. We’ll access this data using the nycflights13 R package"),
                 
                br(), 
                tabsetPanel(
                    id = "tabset1",
                    ###3.1.1 by year-----------------------------------------------
                    tabPanel("By Year", 
                             fluidRow(
                                 valueBoxOutput("onTimePerBox"),
                                 valueBoxOutput("delay_avgminBox"),
                                 valueBoxOutput("delay_medminBox"))),
                    
                    ###3.1.2 by month-----------------------------------------------
                    tabPanel("By Month", 
                             sliderInput("month_31", 
                                         label = h4("Departure Month"), 
                                         min = 1, 
                                         max = 12, 
                                         value = c(1,12))),
                    
                    ###3.1.3 by day------------------------------------------------
                    tabPanel("By Day", 
                             dateInput("date",                       
                                       h4("Date"),
                                       value = "2013-01-01"))
                )
            
            ),
            
            
            ##3.2 Weather--------------------------------------------------------
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
            
                
                plotOutput("line_35"),
                plotOutput("line_35_2")
            )
        )
    )
)
                
                
                
                
            
            
        
    


server <- function(input, output) {
    
    
    ##3.1 dashboard--------------------------------------------------------------
    ###3.1.1 by year
    output$onTimePerBox <- renderValueBox({ 
        valueBox(paste0(all_flights$delay_per ,"%"),
                 "On Time Performance", 
                 icon = icon("calendar"),
                 color = "purple")
    })
    
    output$delay_avgminBox <- renderValueBox({
        valueBox(paste0(all_flights$delay_avg_min ,"min"),
                 "Average Delay Minutes", 
                 icon = icon("clock"),
                 color = "yellow")
    })
    
    output$delay_medminBox <- renderValueBox({
        valueBox(paste0(all_flights$delay_med_min ,"min"),
                 "Median Delay minutes", 
                 icon = icon("clock"),
                 color = "green")
    }) 
    
    
    ##3.5 flights----------------------------------------------------------------
    output$line_35 <- renderPlot({ 
        
        day_flights <- flights %>%
            filter(month == month(input$date_35), day == day(input$date_35)) %>%
            group_by(month, day, hour, origin) %>%
            summarise(count = n()) 
        
        ggplot(day_flights, aes(x=hour, y=count, group=origin)) +
            geom_line(aes(linetype = origin))
    }) 
    
    output$line_35_2 <- renderPlot({ 
        
        month_flights <- flights %>%
            filter(month >= input$month_35[1], month <= input$month_35) %>%
            group_by(month,  origin) %>%
            summarise(count = n()) 
        
        ggplot(month_flights, aes(x=month, y=count, group=origin)) +
            geom_line(aes(linetype = origin))
    })
     
}
    

shinyApp(ui, server)
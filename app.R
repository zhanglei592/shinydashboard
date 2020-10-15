library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2) 
library(lubridate)
library(grid)

library(nycflights13)
load("data.Rdata")


 

ui <- dashboardPage(
    
    skin = "red",
    
    #1.header-------------------------------------------------------------------
    #add nav bar
    dashboardHeader(title = "Basic Dashboard"),
    
    #2.sidebar------------------------------------------------------------------
    dashboardSidebar(
        menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
        menuItem("Weather", icon = icon("cloud-sun-rain"), tabName = "Weather")
    ),
    
    #3.body
    dashboardBody(
        
        tabItems(
            
            ##dashboard------------------------------------------------------
            
            tabItem( 
                
                tabName = "dashboard",
                helpText("Throughout this dashboard, we’re going to analyze data related 
               to all domestic flights departing from one of New York City’s three 
               main airports in 2013. We’ll access this data using the nycflights13 R package"),
                 
                br(), 
                tabsetPanel(
                    id = "tabset1",
                    ###3.1 by year-----------------------------------------------
                    
                    tabPanel("By Year", 
                             br(),
                             fluidRow(
                                 valueBoxOutput("onTimePerBox"),
                                 valueBoxOutput("delay_avgminBox"),
                                 valueBoxOutput("delay_medminBox")),
                             fluidRow(
                                 box(plotOutput("plot311",  height = 300)),
                                 box(plotOutput("plot312",  height = 300))),
                             plotOutput("plot313", height = 600)
                             ),
                    
                    ###3.2 by month-----------------------------------------------
                    tabPanel("By Month", 
                             sliderInput("month_321", 
                                         label = h4("Departure Month"), 
                                         min = 1, 
                                         max = 12, 
                                         value = c(1,12)),
                             plotOutput("line_321")
                             ),
                    
                    ###3.3 by day------------------------------------------------
                    tabPanel("By Day", 
                             br(),
                             dateInput("date_331",h4("Date"),
                                       value = "2013-01-01"),
                             plotOutput("line_331")
                             )
                    
                    
                    
                )
            
            ),
            
            
            ##3.2 Weather--------------------------------------------------------
            tabItem(
                tabName = "Weather",
                helpText("Hourly meteorological data for each of the three NYC airports. 
               This data frame has 26,115 rows.")
            )
        )
    )
)
                
                
                
                
            
            
        
    


server <- function(input, output) {
    
    
    ##3.1 dashboard--------------------------------------------------------------
    ###3.1.1 by year
    output$onTimePerBox <- renderValueBox({ 
        valueBox(paste0(all_flights_summary$delay_per ,"%"),
                 "On Time Performance", 
                 icon = icon("calendar"),
                 color = "purple")
    })
    
    output$delay_avgminBox <- renderValueBox({
        valueBox(paste0(all_flights_summary$delay_avg_min ,"min"),
                 "Average Delay Minutes", 
                 icon = icon("clock"),
                 color = "yellow")
    })
    
    output$delay_medminBox <- renderValueBox({
        valueBox(paste0(all_flights_summary$delay_med_min ,"min"),
                 "Median Delay minutes", 
                 icon = icon("clock"),
                 color = "green")
    }) 
    
    output$plot311 <- renderPlot({
        
        p_month_flights_origin_summary <- month_flights_origin_summary %>%
            left_join(airports, by = c("origin"="faa"))  
        
        ggplot(data = p_month_flights_origin_summary, 
               mapping = aes(x = month, y = delay_per ,group = name,color = name))  %>%
            + geom_line()   %>%  
            + scale_x_continuous(breaks = c(3,6,9,12)) %>% 
            + scale_y_continuous(name = "On Time percent", 
                                 labels = function(delay_per) { paste0(round(delay_per, 1), "%")}) %>%
            + labs(
                title = "The Proportion of Flights Arriving On Time",
                subtitle = "Departing from New York Airport in 2013", 
                caption = "datasource: nycflights13" ) %>%
            +theme(legend.position = "top") %>%
            + scale_color_discrete(name = "Airports") 
    })
    
    
    output$plot312 <- renderPlot({
        p_month_flights_origin_summary <- month_flights_origin_summary %>%
            left_join(airports, by = c("origin"="faa"))  
        
        ggplot(data = p_month_flights_origin_summary, 
               mapping = aes(x = month, y = delay_med_min,group = name,color = name))  %>%
            + geom_line()   %>%  
            + scale_x_continuous(breaks = c(3,6,9,12)) %>% 
            + scale_y_continuous(name = "Median Delay By Minutes") %>%
            + labs(
                title = "The Median Delay of Flights By Minutes",
                subtitle = "Departing from New York Airport in 2013", 
                caption = "datasource: nycflights13" ) %>%
            +theme(legend.position = "top") %>%
            + scale_color_discrete(name = "Airports")
    })
    
    output$plot313 <- renderPlot({
        
        p_month_flights_carriers_summary <- month_flights_carriers_summary %>%
            inner_join(airlines, by = "carrier")  
        
        
        ggplot(data = p_month_flights_carriers_summary)  %>%
            + geom_line(aes(
                y = delay_med_min,
                x = month))   %>% 
            + geom_line(aes(
                y = delay_per, 
                x = month), color = "blue", linetype = 2) %>% 
            + scale_x_continuous(breaks = c(3,6,9,12))  %>%
            + scale_y_continuous(name = "Median Delay By Minutes", 
                                 sec.axis = sec_axis(~., name = "On Time percent", 
                                                     labels = function(delay_per) { paste0(round(delay_per, 1), "%")})) %>%
            + theme( 
                axis.title.y.right = element_text(color = "blue")) %>%
            + facet_wrap(~ name, ncol=4) %>%
            + labs(
                title = "The proportion of airline flights arriving on time and the median delay minutes",
                subtitle = "Departing from New York Airport in 2013", 
                caption = "datasource: nycflights13" 
            )
    })
    
    
    output$line_321 <- renderPlot({ 
        
        month_flights <- new_flights %>%
            filter(month >= input$month_321[1], month <= input$month_321[2]) %>%
            group_by(month,  origin) %>%
            summarise(count = n()) 
        
        ggplot(month_flights, aes(x=month, y=count, group=origin)) +
            geom_line(aes(linetype = origin))
    })
    
  
    output$line_331 <- renderPlot({ 
        
        day_flights <- new_flights %>%
            filter(month == month(input$date_331), day == day(input$date_331)) %>%
            group_by(month, day, hour, origin) %>%
            summarise(count = n()) 
        
        ggplot(day_flights, aes(x=hour, y=count, group=origin)) +
            geom_line(aes(linetype = origin))
    }) 
    
    
     
}
    

shinyApp(ui, server)
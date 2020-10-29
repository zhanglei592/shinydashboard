library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2) 
library(lubridate)
library(grid) 
library(ggiraph)
library(plotly)

library(nycflights13)
library(dashboardthemes)
load("data.Rdata")


 

ui <- dashboardPage(
    skin = "blue",
    
    #1.header-------------------------------------------------------------------
    #add nav bar
    dashboardHeader(title = "On-Time Performance"),
    
    #2.sidebar------------------------------------------------------------------
    dashboardSidebar(
        sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")) 
        )
    ),
    
    #3.body
    dashboardBody(
        
        ### changing theme
        shinyDashboardThemes( theme = "grey_light" ),
        
        
        ### body
        tabItems(
            
            ##dashboard------------------------------------------------------
            
            tabItem( 
                
                tabName = "dashboard",
                helpText("Throughout this dashboard, we’re going to analyze data related 
               to all domestic flights departing from one of New York City’s three 
               main airports in 2013. We’ll access this data using the nycflights13 R package"),
                helpText("A flight is considered delayed when it arrived 15 or more minutes than the schedule.
                         Delayed minutes are calculated for delayed flights only. 
                         Data presented summarizes arriving flights only. "), 
                br(), 
                radioButtons("Airports", 
                             choices = list("Three main airports over all" = "All",
                                            "Newark Liberty Intl" = "EWR",
                                            "John F Kennedy Intl" = "JFK", 
                                            "La Guardia" = "LGA"),
                             label = "Choose departure airport",
                             inline = TRUE,
                             selected = "All"),
                br(),
                fluidRow(valueBoxOutput("onTimePerBox"),
                         valueBoxOutput("delay_avgminBox"),
                         valueBoxOutput("delay_medminBox")),
                fluidRow(box(plotOutput("plot311",  height = 300),status = "success"),
                         box(plotOutput("plot312",  height = 300),status = "warning")),
                plotOutput("plot313", height = 600 )
                ) 
            
            
            
        )
    )
)
                
                

server <- function(input, output) {
    
    

#3.1 by year---------------------------------------
    
    ##valuebox------
    output$onTimePerBox <- renderValueBox({ 
        
        a_all_summary <- all_summary %>%
            dplyr::filter(
                if(input$Airports == "All"){ categary == "summary" } 
                else { categary == "origin" & sub == input$Airports }) %>% 
            select(on_time_rate) 
         
        valueBox(paste0(a_all_summary$on_time_rate ,"%"),
                 "On Time Rate", 
                 icon = icon("calendar"),
                 color = "purple") 
    })
    
    output$delay_avgminBox <- renderValueBox({
        
        b_all_summary <- all_summary %>%
            dplyr::filter(
                if(input$Airports == "All"){ categary == "summary" } 
                else { categary == "origin" & sub == input$Airports }) %>% 
            select(delay_avg_min) 
        
        valueBox(paste0(b_all_summary$delay_avg_min ,"min"),
                     "Average Delay by Minutes", 
                     icon = icon("clock"),
                     color = "yellow")
        
    })
    
    output$delay_medminBox <- renderValueBox({
        
        c_all_summary <- all_summary %>%
            dplyr::filter(
                if(input$Airports == "All"){
                    categary == "summary"
                } else {
                    categary == "origin" & sub == input$Airports }) %>% 
            select(delay_med_min) 
        
        
            valueBox(paste0(c_all_summary$delay_med_min ,"min"),
                     "Median Delay by Minutes", 
                     icon = icon("clock"),
                     color = "green") 
    }) 
    
    ##On Time percent plot-------------------------
    output$plot311 <- renderPlot({
        
        a_month_summary <- month_summary %>%
            dplyr::filter(categary == "origin" & sub == input$Airports )  
        
        b_month_summary <- month_summary %>%
            dplyr::filter( categary == "summary") 
        
        b_month_summary$name = "Over All"
        
        
        ggplot(  )  %>% 
            + geom_line( data = b_month_summary,aes(x = month, y = on_time_rate ,color = name)) %>% 
            + geom_line( data = a_month_summary,aes(x = month, y = on_time_rate ,group = name,color = name))   %>%
            + scale_x_continuous(breaks = c(3,6,9,12)) %>% 
            + scale_y_continuous(name = "On Time Rate", 
                                 labels = function(on_time_rate) { paste0(round(on_time_rate, 1), "%")}) %>%
            + labs(
                title = "Flights Arriving On Time Rate",
                subtitle = "Departing from New York Airport in 2013", 
                caption = "datasource: nycflights13" ) %>%
            +theme(legend.position = "top") %>%
            + scale_color_discrete(name = "Airports") 
        
        
    })
    
    ##Median Delay By Minutes plot-----------------
    output$plot312 <- renderPlot({ 
        
        a_month_summary <- month_summary %>%
            dplyr::filter(categary == "origin" & sub == input$Airports )  
        
        b_month_summary <- month_summary %>%
            dplyr::filter( categary == "summary") 
        
        b_month_summary$name = "Over All"
        
        ggplot(  )  %>% 
            + geom_line( data = b_month_summary,aes(x = month, y = delay_med_min ,group = name,color = name)) %>%  
            + geom_line( data = a_month_summary,aes(x = month, y = delay_med_min ,group = name,color = name))   %>%   
            + scale_x_continuous(breaks = c(3,6,9,12)) %>% 
            + scale_y_continuous(name = "Median Delay By Minutes") %>%
            + labs(
                title = "Median Delay Per Delayed Arrival By Minutes",
                subtitle = "Departing from New York Airport in 2013", 
                caption = "datasource: nycflights13" ) %>%
            +theme(legend.position = "top") %>%
            + scale_color_discrete(name = "Airports") 
        
    })
    
    ##airline flights on time and delay median plot -------------------------
    output$plot313 <- renderPlot({ 
        if(input$Airports == "All"){
            a_month_summary <- month_summary %>%
                dplyr::filter(categary == "carrier")  
        } else {
            a_month_summary <- month_flights_carriers_origin_summary %>%
                dplyr::filter(origin == input$Airports) }
         
            
        
        ggplot(data = a_month_summary)  %>%
            + geom_line(aes( y = delay_med_min, x = month))   %>% 
            + geom_line(aes( y = on_time_rate,  x = month), color = "blue", linetype = 2) %>% 
            + scale_x_continuous(breaks = c(3,6,9,12))  %>%
            + scale_y_continuous(name = "Median Delay By Minutes", 
                                 sec.axis = sec_axis(~., name = "On Time Rate", 
                                                     labels = function(on_time_rate) { paste0(round(on_time_rate, 1), "%")})) %>%
            + theme( 
                axis.title.y.right = element_text(color = "blue")) %>%
            + facet_wrap(~ if(input$Airports == "All"){name} else {carrier_name}, ncol=4) %>%
            + labs(
                title = "Proportion of Airline flights ",
                subtitle = "Departing from New York Airport in 2013", 
                caption = "datasource: nycflights13" 
            )
         
        
    })
     
     
}
    

shinyApp(ui, server)
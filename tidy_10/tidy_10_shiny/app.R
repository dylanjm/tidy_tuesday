#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(glue)

# TRIP CLEAN WAS TOO LARGE FOR GITHUB, RUN THE BELOW CODE TO EFFECTIVELY GET
# THE SAME TRIP CLEAN DATA I AM USING. 

# # Read in data supa fast w/ data.table
# trip_data <- list.files(here::here("data/PublicTripData"), 
#                         pattern = "*.csv", full.names = TRUE) %>% 
#   map_df(~data.table::fread(.x))
# 
# # Clean up data
# trip_clean <- trip_data %>% 
#   filter(StartDate != "", StartTime != "") %>%
#   mutate(Start = parse_date_time(glue("{StartDate} {StartTime}"), "mdY HM"),
#          Hour = parse_factor(hour(Start), c(0,23:1)),
#          Weekday = fct_relevel(wday(Start, label = TRUE),
#                                c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
#
# write_rds(trip_clean, here::here("data/trip_clean.rds"))

trip_clean <- read_rds(here::here("data/trip_clean.rds"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   img(src = "biketown.png"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("topn", "Top N Start Locations", 148, min = 1, max = 148),
        selectInput("pmt", "Payment Plan", choices = c("Both", 
                                                       "Casual", "Subscriber")),
        dateRangeInput("daterng", "Select Start & End Dates",
                       start = as_datetime("2016-07-19"), end = as_datetime("2018-03-31")), 
        htmlOutput("trips"),
        htmlOutput("duration")
      ),
      
      

      # Show a plot of the generated distribution
      mainPanel(
        h1("Starting Hubs"),
         fluidRow(leafletOutput("LeafyMap")),
         fluidRow(column(width = 7, plotOutput("TimePlot")),
                  column(width = 5, plotOutput("TilePlot")))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$LeafyMap = renderLeaflet({
    tc <- trip_clean %>% 
      filter(between(Start, as_datetime(input$daterng[1]), 
                     as_datetime(input$daterng[2]))) %>% 
      filter(PaymentPlan %in% ifelse(input$pmt == "Both", c("Casual", "Subscriber"),
                                      input$pmt)) %>% 
      filter(!StartHub == "") %>%
      select(StartHub, StartLatitude, StartLongitude) %>% 
      add_count(StartHub) %>% 
      distinct() %>% 
      top_n(input$topn, n)
    
      leaflet(tc) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircles(lng = ~StartLongitude, lat = ~StartLatitude, radius = ~n/30,
                 color = "white", opacity = 1, fillColor = "#f94d1f",
                 fillOpacity = 1, weight = 2)
  })
  
  output$TimePlot = renderPlot({
    trip_clean %>% 
      filter(between(Start, as_datetime(input$daterng[1]), 
                     as_datetime(input$daterng[2]))) %>% 
      mutate(floor_week = floor_date(Start, "weeks")) %>% 
      add_count(floor_week, PaymentPlan) %>% 
      add_count(floor_week) %>% 
      filter(PaymentPlan %in% "Subscriber") %>% 
      ggplot() + 
      geom_ribbon(aes(x = floor_week, ymin = 0, ymax = nn), 
                  fill = "grey", color = "grey50") +
      geom_ribbon(aes(x = floor_week, ymin = 0, ymax = n),
                  fill = "#f69366", color = "grey50") +
      scale_y_continuous(labels = glue::glue("{seq(0,15,5)}K")) + 
      scale_x_datetime(date_labels = "%b %y", 
                       breaks = seq(as_datetime("2016-08-01"), 
                                    as_datetime("2018-02-01"), "6 months")) + 
      labs(title = "Trips Per Week") + 
      theme_minimal() + 
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(family = "Verdana Bold", 
                                      size = 18, margin = margin(b = 20)),
            axis.text = element_text(family = "Futura Medium", size = 13))
  })
  
  output$TilePlot = renderPlot({
    trip_clean %>% 
      filter(between(Start, as_datetime(input$daterng[1]), 
                     as_datetime(input$daterng[2]))) %>% 
      filter(PaymentPlan %in% ifelse(input$pmt == "Both", c("Casual", "Subscriber"),
                                     input$pmt)) %>%
      count(Weekday, Hour) %>%
      filter(!Hour %in% c(1,2,3,4)) %>% 
      ggplot(aes(x = Weekday, y = Hour, fill = n)) + 
      geom_tile() +
      annotate("segment", y = 21, yend = 21, x = -Inf, xend = Inf,
               color = "black", size = .3) +
      annotate("segment", y = 0, yend = 0, x = -Inf, xend = Inf,
               color = "black", size = .3) +
      scale_x_discrete(position = "top") + 
      scale_y_discrete(labels = c("12 AM",glue("{c(11:1,12)} PM"), glue("{11:5} AM"))) +
      scale_fill_gradient(low = "white", high = "#f94d1f") + 
      labs(title = "Trips per Weekday/Hour") + 
      theme_minimal() + 
      theme(legend.position = "none",
            axis.title = element_blank(),
            panel.grid = element_blank(), 
            axis.text.y = element_text(margin = margin(r = 20)),
            plot.title = element_text(family = "Verdana Bold", size = 18),
            axis.text = element_text(family = "Futura Medium"))
  })
  
  output$trips = renderText({
    t <- trip_clean %>% 
      filter(between(Start, as_datetime(input$daterng[1]), 
                     as_datetime(input$daterng[2]))) %>% 
      filter(PaymentPlan %in% ifelse(input$pmt == "Both", c("Casual", "Subscriber"),
                                     input$pmt)) %>%
      count() %>% 
      {paste("Number of Trips Taken:", .)}
    
    paste(t)
  })
  
  # If anybody could help me figure out how to take mean of time objects
  output$duration = renderText({
    d <- trip_clean %>% 
      filter(between(Start, as_datetime(input$daterng[1]), 
                     as_datetime(input$daterng[2]))) %>% 
      filter(PaymentPlan %in% ifelse(input$pmt == "Both", c("Casual", "Subscriber"),
                                     input$pmt)) %>%
      {mean(.$Duration, na.rm = TRUE)}
    
    paste("Average Duration per Trip:", d)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


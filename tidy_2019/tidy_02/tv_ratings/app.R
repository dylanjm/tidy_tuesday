#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(ggrepel)
library(ggsci)

# Grab my predownloaded data
tv_dat <- read_csv("week02_tv_ratings.csv") %>% 
    mutate(title = case_when(
        title == "Twin Peaks" & date > as.Date("2010-01-01") ~ "Twin Peaks\n(reboot)", 
        title == "The X-Files" & date > as.Date("2010-01-01") ~ "The X-Files\n(reboot)",
        TRUE ~ title
    ))

select_shows <- tv_dat %>% 
    pull(title) %>% 
    unique()

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(HTML("<center>
                    <h4><p style='color:#ff5c33'>The end of channel surfing</p></h4>
                    <p>TV's golden age is real</p>
                    <h5><p style='font-style:italic'>But for every Breaking Bad, more shows are just bad</p></h5>
                    </center>")),
    
    
    column(12, selectInput("tv_shows", 
                "Select a TV Show", 
                choices = c("Choose" = "",
                            select_shows),
                multiple = TRUE,
                width = "100%")
           ),
    
    # Show a plot of the generated distribution
    plotOutput("econPlot", 
               width = "100%",
               height = "800px"),
    HTML("<br><br><br><br>
         <center>
         <h1>How Do Your Favorites Fare Over Time?</h1>
         <h4><p style='color:#ff5c33; font-style:italic'>Not every show can be Game of Thrones</p></h4>"),
    plotOutput("ratingsPlot", 
               width = "100%",
               height = "800px")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$econPlot <- renderPlot({
        selected_shows <- input$tv_shows
        highlight_shows <- tv_dat %>% 
            filter(title %in% selected_shows)
        
        labs <- highlight_shows %>% 
            group_by(title) %>% 
            filter(row_number(title) == 1)
        
        tv_dat %>% 
            ggplot(aes(x = date, y = av_rating, size = share)) + 
            geom_point(color = "#d7ebf2", alpha = .8) + 
            geom_smooth(method = "lm", se = F, linetype = "dashed", 
                        color = "skyblue4") + 
            geom_smooth(aes(weight = share), method = "lm", se = F, 
                        color = "gold4") + 
            annotate(geom = "text", x = as.Date("2015-01-01"), y = 8.25, 
                     label = "TV drama trend", color = "skyblue4", 
                     fontface = "bold", size = 5) + 
            annotate(geom = "text", x = as.Date("2015-01-01"), y = 8.65, 
                     label = "TV drama trend\nweighted by share", color = "gold4", 
                     fontface = "bold", size = 5) + 
            geom_point(data = highlight_shows, aes(x = date, y = av_rating, size = share,
                                                   color = title)) + 
            geom_line(data = highlight_shows, aes(x = date, y = av_rating, group = title,
                                                  color = title), inherit.aes = FALSE) + 
            geom_text_repel(data = labs, aes(x = date, y = av_rating, label = title,
                                             color = title),
                            inherit.aes = FALSE, nudge_y = .25,
                            fontface = "bold", size = rel(6)) + 
            coord_cartesian(ylim = c(5.5, 9.5)) + 
            scale_y_continuous(breaks = seq(5.5, 9.5, .5), position = "right")+
            scale_x_date(breaks = as.Date(c("1990-01-01",
                                            "1995-01-01", "2000-01-01",
                                            "2005-01-01", "2010-01-01",
                                            "2015-01-01", "2018-01-01")), 
                         date_labels = "%Y") +
            scale_size_continuous(range = c(1,12)) + 
            scale_color_futurama() +
            labs(caption = "*Seasons with at least 100 ratings on average\n*Size=Share of IMDb ratings for shows that year") +
            theme_minimal() + 
            theme(plot.title = element_text(hjust = .5, size = rel(2.5)),
                  plot.subtitle = element_text(hjust = .5, face = "italic", size = rel(1.3)),
                  plot.caption = element_text(face = "italic", color = "grey60", 
                                              size = rel(1)), 
                  axis.title = element_blank(),
                  axis.text = element_text(size = rel(1.2)),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(), 
                  legend.position = "none")
    })
    
    output$ratingsPlot <- renderPlot({
        selected_shows <- input$tv_shows
        highlight_shows <- tv_dat %>% 
            filter(title %in% selected_shows)
        
        labs <- highlight_shows %>% 
            group_by(title) %>% 
            filter(row_number(title) == max(row_number(title)))
        
        highlight_shows %>% 
            ggplot(aes(x = seasonNumber, y = av_rating, group = title, 
                       color = title)) + 
            geom_line() + 
            geom_point(aes(size = share)) + 
            geom_text_repel(data = labs, aes(x = seasonNumber, y = av_rating, label = title,
                                             color = title),
                            segment.size = 0,
                            inherit.aes = FALSE, nudge_y = .005,
                            nudge_x = .3,
                            fontface = "bold", size = rel(6)) +
            scale_y_continuous(breaks = seq(0,10,.1)) + 
            scale_x_continuous(breaks = 1:20) + 
            scale_color_futurama() + 
            labs(x = "Season", y = "Avg. Rating") + 
            theme_minimal() + 
            theme(plot.title = element_text(hjust = .5, size = rel(2.5)),
                  plot.subtitle = element_text(hjust = .5, face = "italic", size = rel(1.3)),
                  plot.caption = element_text(face = "italic", color = "grey60", 
                                              size = rel(1)), 
                  axis.text = element_text(size = rel(1.2)),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(), 
                  legend.position = "none")
            
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

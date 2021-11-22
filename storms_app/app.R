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
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(knitr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Storms Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("storm",
                        "Name of storm:",
                        unique(paste(storms$name, storms$year))),
        sliderInput("bounds",
                    "Map boundary amount",
                    min = 1,
                    max = 20,
                    value = 5),
        radioButtons("dataSum",
                     "What to display below plot",
                     choices = c("nothing", "underlying data", "data summary"))
        
        ),
        
        
     
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mapPlot"),
            verbatimTextOutput("summary"),
            dataTableOutput("data_table")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$mapPlot <- renderPlot({
        world <- ne_countries(scale = "medium", returnclass = "sf") 
        
        storm_ind <- dplyr::filter(storms, name == str_extract(input$storm, "[:alpha:]+[:digit:]*"), 
                                   year == str_trim(str_extract(input$storm, "\\s[:digit:]+")))
        
        amt <- input$bounds
        
        longs <- c(min(storm_ind$long) - amt, max(storm_ind$long) + amt)
        lats <- c(min(storm_ind$lat) - amt, max(storm_ind$lat) + amt)
        
        ggplot(data = world) +
            geom_sf() +
            coord_sf(xlim = longs, ylim = lats, expand = FALSE) +
            geom_point(data = storm_ind, aes(long, lat, color = wind, size = ifelse(!is.na(hu_diameter), hu_diameter + 1, 2))) +
            guides(size = "none") +
            scale_color_gradient(low = "#fee5d9", high = "#99000d") + 
            theme_bw()
    })
    
    output$data_table <- renderDataTable({
        storm_ind <- dplyr::filter(storms, name == str_extract(input$storm, "[:alpha:]+[:digit:]*"), 
                                   year == str_trim(str_extract(input$storm, "\\s[:digit:]+")))
        
        if(input$dataSum == "underlying data"){
            storm_ind
        }
    })
    
    output$summary <- renderPrint({
        storm_ind <- dplyr::filter(storms, name == str_extract(input$storm, "[:alpha:]+[:digit:]*"), 
                                   year == str_trim(str_extract(input$storm, "\\s[:digit:]+")))
        
        if(input$dataSum == "data summary"){
            summary(storm_ind)
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

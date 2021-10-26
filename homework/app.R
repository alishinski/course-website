#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sjPlot)
library(broom)
library(lmSupport)
library(car)
library(here)
library(tidyverse)

data <- read_csv(here("content", "data", "undergrad_data.csv"))
data <- data[,-c(1:2)]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regression Modeler"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            selectInput("dv",
                        "Dependent Variable",
                        colnames(data)),
            
            checkboxGroupInput("ivs",
                               "independent variables",
                               choices = colnames(data)),
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("message"),
           tableOutput("coefTable"),
           tableOutput("modelFit"),
           plotOutput("regPlot")
        )
    )
)

# Define server logic to run regression models and produce output
server <- function(input, output) {

    output$coefTable <- renderTable({
        
        rhs <- paste(input$ivs, collapse = " + ")
        
        formula <- paste(input$dv, rhs, sep = " ~ ")
        
        fit <- lm(formula, data = data)

        tidy(fit)
        

    })
    
        
    observeEvent(input$ivs, { message("model has been fit")})
    
    
    observe({"The Model Has Been FIt"}) %>% bindEvent(input$ivs)#, {renderText("The Model has been Fit")})
    
    
    output$modelFit <- renderTable({
        
        rhs <- paste(input$ivs, collapse = " + ")
        
        formula <- paste(input$dv, rhs, sep = " ~ ")
        
        fit <- lm(formula, data = data)
        
        glance(fit)
    })
    
    output$regPlot <- renderPlot({
        
        rhs <- paste(input$ivs, collapse = " + ")
        
        formula <- paste(input$dv, rhs, sep = " ~ ")
        
        fit <- lm(formula, data = data)
        
        ggplot(model.frame(fit)) + 
            geom_point(aes(x = fitted(fit), y = residuals(fit)))
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

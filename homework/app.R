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

data <- read_csv(here("undergrad_data.csv"))
data <- data[,-c(1:2)]

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly"),

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
            
            actionButton("fit",
                         "Fit the Model")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
                    tabsetPanel(
                        tabPanel(
                            p("Coefficients"),
                            tableOutput("coefTable")
                        ),
                        tabPanel(
                            p("Model Summary"),
                            tableOutput("modelFit")
                        ),
                        tabPanel(
                            p("Fitted vs residuals"),
                            plotOutput("regPlot", click = "plot_click"),
                            verbatimTextOutput("click")
                        )
                        
                        
                    )

                )
            )
)


# Define server logic to run regression models and produce output
server <- function(input, output) {
    
    rhs <- reactive({paste(input$ivs, collapse = " + ")})
    
    formula <- reactive({paste(input$dv, rhs(), sep = " ~ ")})
    
    fit <- eventReactive(input$fit, {
        lm(formula(), data = data)
    })

    output$coefTable <- renderTable({

        tidy(fit())
        
    })
    
    
    output$modelFit <- renderTable({
        
        glance(fit())
    })
    
    output$regPlot <- renderPlot({
        
        ggplot(model.frame(fit())) + 
            geom_point(aes(x = fitted(fit()), y = residuals(fit())))
        
    })
    
    output$click <- renderPrint({
        req(input$plot_click)
        x <- input$plot_click$x
        y <- input$plot_click$y
        cat("Fitted: ", x, "Residual: ", y)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

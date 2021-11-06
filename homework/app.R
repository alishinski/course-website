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

undergrad_data <- read_csv(here("undergrad_data.csv"))
undergrad_data <- undergrad_data[,-c(1:2)]

tidykids <- read_csv(here("tidykids.csv"))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    shinyFeedback::useShinyFeedback(),

    # Application title
    titlePanel("Regression Modeler"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            textInput("data",
                      "Which Dataset to Use?"),

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
                        ),
                        tabPanel(
                            p("Data"),
                            dataTableOutput("dataframe")
                        )
                        
                        
                    )

                )
            )
)


# Define server logic to run regression models and produce output
server <- function(input, output) {
    
    data <- reactive({
        req(input$data)
        
        exists <- exists(input$data)
        shinyFeedback::feedbackDanger("data", !exists, "Unknown dataset")
        req(exists, cancelOutput = TRUE)
        
        get(input$data)
        })
    
    observeEvent(data(), {
        choices <- unique(colnames(data()))
        updateSelectInput(inputId = "dv", choices = choices)
        updateCheckboxGroupInput(inputId = "ivs", choices = choices) 
    })
    
    rhs <- reactive({paste(input$ivs, collapse = " + ")})
    
    formula <- reactive({
        
        proj_double <- input$dv == "PROJ_TOTAL" & sum(str_detect(input$ivs, "Proj")) > 0

        shinyFeedback::feedbackWarning("dv", proj_double, "You may not want to regress PROJ_TOTAL on other project scores.")
        paste(input$dv, rhs(), sep = " ~ ")})
    
    fit <- eventReactive(input$fit, {
        lm(formula(), data = data())
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

    output$dataframe <- renderDataTable({
        data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

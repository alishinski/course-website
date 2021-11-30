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
library(plotly)

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
            
            fileInput("newdata",
                      "Upload data set"),
            
            selectInput("dv",
                        "Dependent Variable",
                        colnames(data)),
            
            checkboxGroupInput("ivs",
                               "independent variables",
                               choices = colnames(data)),
            
            actionButton("fit",
                         "Fit the Model"),
            
            selectInput("newbutton",
                        "Should there be a new button?",
                        c("", "Yes", "No")), 
            

            uiOutput("button")
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
                    plotOutput("regPlot", hover = "plot_hover"),
                    verbatimTextOutput("hover")
                    #tableOutput("brush")
                ),
                tabPanel(
                    p("Data"),
                    dataTableOutput("dataframe")
                ),
                tabPanel(
                    p("plotly"),
                    plotlyOutput("plotly")
                )
                
            )
            
        )
    )
)


# Define server logic to run regression models and produce output
server <- function(input, output) {
    
    # Preloaded data
    typedata <- reactive({
        req(input$data)

        exists <- exists(input$data)
        shinyFeedback::feedbackDanger("data", !exists, "Unknown dataset")
        req(exists, cancelOutput = TRUE)

        get(input$data)
    })

    
    # Uploaded data
    newdata <- reactive({
        req(input$newdata)
        
        df <- read.csv(input$newdata$datapath)
        return(df)

    })
    
    d <- reactiveValues(data = NULL)
    
    observeEvent(input$newdata, {
        
        d$data <- newdata()
        
    }
    )
    
    observeEvent(input$data, {
        
        d$data <- typedata()
        
    }
    )
    
    observeEvent(d$data, {
        choices <- unique(colnames(d$data))
        updateSelectInput(inputId = "dv", choices = choices)
        updateCheckboxGroupInput(inputId = "ivs", choices = choices) 
    })
    
    iv_limit <- reactiveVal()
    
    observeEvent(d$data, {
        
        iv_limit(ncol(d$data) / 2)
        
    })
    
    rhs <- reactive({paste(input$ivs, collapse = " + ")})
    
    formula <- reactive({
        
        proj_double <- input$dv == "PROJ_TOTAL" & sum(str_detect(input$ivs, "Proj")) > 0
        
        too_many_ivs <- length(input$ivs) > iv_limit()
        
        shinyFeedback::feedbackWarning("data", too_many_ivs, "You may not want to use more than half of the columns as IVs")
        paste(input$dv, rhs(), sep = " ~ ")})
    
    fit <- eventReactive(input$ok, {
        
        if(input$dv %in% input$ivs){
            validate("Dependent variable should not be an independent variable")
        }
        
        lm(formula(), data = d$data)
    })
    
    observeEvent(fit(), {
        showNotification("The model has been fit!", duration = 2, type = "error")
        Sys.sleep(2)
        showNotification("You did a good job!")
    })
    
    
    modal_confirm <- modalDialog(
        "Are you sure you want to fit the model?",
        title = "fit model",
        footer = tagList(
            actionButton("cancel", "Cancel"),
            actionButton("ok", "Fit", class = "btn btn-danger")
        )
    )
    
    observeEvent(input$fit, 
                 {showModal(modal_confirm)})
    
    observeEvent(input$cancel, {
        removeModal()
    })
    
    observeEvent(input$ok, {
        removeModal()
    })
    
    output$coefTable <- renderTable({
        
        tidy(fit())
        
    })
    
    output$modelFit <- renderTable({
        
        glance(fit())
        
    })
    
    output$regPlot <- renderPlot({
        
        df <- data.frame(fitted = fit()$fitted, resid = fit()$residuals)
        
        ggplot(df) + 
            geom_point(aes(x = fitted, y = resid))
        
    })
    
    output$hover <- renderPrint({
        req(input$plot_hover)
        x <- input$plot_hover$x
        y <- input$plot_hover$y
        cat("Fitted: ", x, "Residual: ", y)
    })
    
    
    output$points <- renderTable({
        req(input$plot_click)
        df <- data.frame(fitted = fit()$fitted, resid = fit()$residuals)
        nearPoints(df, input$plot_click)
        
    })
    
    output$brush <- renderTable({
        req(input$plot_brush)
        df <- data.frame(fitted = fit()$fitted, resid = fit()$residuals)
        brushedPoints(df, input$plot_brush)
        
    })
    
    output$dataframe <- renderDataTable({
        d$data
    })
    
    output$button <- renderUI({
        if(input$newbutton == "Yes"){
            actionButton("push", "Push this button")
        } else {
            NULL
        }
    })
    
    output$plotly <- renderPlotly({
        
        df <- data.frame(fitted = fit()$fitted, resid = fit()$residuals)
        
        plot_ly(data = df,
                x = ~fitted,
                y = ~resid,
                type = "scatter",
                mode = "markers")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

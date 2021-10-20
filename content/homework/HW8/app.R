#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
# You'll need other packages, load them here

# You can load your chosen dataset here outside the ui and server components and it will be available everywhere
data <- read_csv()


ui <- fluidPage(
    
    # Application title - Change this to include your name
    titlePanel("Regression Modeler"),
    
    # Sidebar layour
    sidebarLayout(
        sidebarPanel(
            
            # put your input components here, a dropdown for the dependent variable, and a checkbox for the independent variables
            # make sure the input ids for these align with what they are called in the server code below (you can keep what I called them or change it just make sure they align)

            
            
            
            
        ),
        
        # Show your different output components here, remember each different render function has a corresponding output function
        # Also remember to use the correct output names from the server code below
        mainPanel(

        )
    )
)

# Define server logic to run regression models and produce output
server <- function(input, output) {
    
    ## Put your code for fitting the regression model here. creating the model formula is a bit tricky so I provided that for you
    output$coefTable <- renderTable({
        
        rhs <- paste(input$ivs, collapse = " + ")
        formula <- paste(input$dv, rhs, sep = " ~ ")
        
        fit <- lm(formula)

    })
    
    ## Create another output table like the one above, but for the model fit statistics for your model
    
    
    
    
    
    
    
    
    ## Put your code for plotting the model fitted values against the model residuals here
    output$regPlot <- renderPlot({
        

    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

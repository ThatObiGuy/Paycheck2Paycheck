# Paycheck - 2 - paycheck
# Owen F. O'Connor

library(shiny)
library(shinythemes) # Adding themeing package, quick way to make app more appealing with minimal effort

# Define UI for application
ui <- fluidPage(theme = shinytheme("superhero"), # Implementation of shinythemes library called above - I like the superhero theme, found @ https://rstudio.github.io/shinythemes/
                
    navbarPage(
      title = "Paycheck2Paycheck", # Text displayed in the top left corner
      
      tabPanel("Forecasting", # This is the title of this panel - the spending forecast happens here
               
               sidebarPanel( # Our "input" panel
                 width = 3, # spacing, allowing for the 3 side-by-side segments
                 
                 tags$h3("Parameters:"), # Here we'll prompt for paycheck amount and days till next, these will help us construct the graph
                 numericInput("paycheck", "Paycheck amount", value = 0, min = 0, max = 5000), # Must be non-negative
                 numericInput("days", "Days till next paycheck", value = 0, min = 0, max = 365), # Also must be non-negative
                 actionButton("buildGraphButton", "Build Graph", class = "btn-lg btn-success"), # Button 'locks-in' entered values
                 
                 tags$h3("Daily income/expenditure:"), # Here we'll take in data on spending/earning that will populate the graph
                 numericInput("moneyMove", "Money Move", value = 0, min = -1000, max = 1000), # Money can move in or out of ones account.
                 actionButton("moneyMoveButton", "Commit", class = "btn-lg btn-success"), # Button 'locks-in' entered value for a given day
                 
               ), # sidebarPanel - input ends
               
               mainPanel(
                 width = 6, # Spacing, allowing for the 3 side-by-side segments
                 h1("This Paycheck:"),
                 
                 plotOutput("spendPlot"), # Displays plot generated on server side.

               ), # mainPanel ends
               
               sidebarPanel( # Our "image" panel
                 width = 3, # spacing, allowing for the 3 side-by-side segments
                 
                 h4("How we're feeling:"),
                 img(src="https://dudewipes.com/cdn/shop/articles/gigachad.jpg?v=1667928905&width=1024",
                     height = 150,
                     width = 300), # Placeholder image for now. - should react to lm fit
                 
               ), # sidebarPanel - image  ends
               
      ), # tabPanel - Forecasting  ends
      
      tabPanel("About", "This panel will be developed in time"),
  
    ) # navbarPage ends
    
  ) # fluidPage ends




# Define server logic required to plot the spending
server <- function(input, output) {
  
  # Reactive values to store inputs
  user_data <- reactiveValues(days = NULL, paycheck = NULL, moneyMoves = numeric())
  
  observeEvent(input$buildGraphButton, { # observeEvents looks for 'build graph' button presses, at which point it can update the stored values
    user_data$days <- input$days
    user_data$paycheck <- input$paycheck
    user_data$moneyMoves <- numeric() # Reset moneyMoves when building a new graph
  })
  
  observeEvent(input$commit, { # Appends moneyMoves vector with new values, each ideally for consecutive days
    user_data$moneyMoves <- c(user_data$moneyMoves, input$moneyMoves)
  })

    output$spendPlot <- renderPlot({
      
      req(user_data$days, user_data$paycheck) # Cannot build graph without these parameters
      
      x <- seq(1, user_data$days, 1)
      y <- numeric(length(x)) # Initialize y as a numeric vector of zeros
      
      plot(x, y, type = "n",
           ylim = c(0, user_data$paycheck * 1.2), 
           xlab = "Days",
           ylab = "Balance")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

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
                 tags$h3("Input:"),
                 numericInput("num", "Money Move", value = 0, min = -1000, max = 1000),
                 actionButton("click", "Commit", class = "btn-lg btn-success"),
                 
                 # not functional yet, but looks semi-decent
                 
               ), # sidebarPanel - input ends
               
               mainPanel(
                 width = 6, # spacing, allowing for the 3 side-by-side segments
                 h1("This Paycheck:"),
                 
                 plotOutput("spendPlot"), # currently just some placeholder data from LM module

               ), # mainPanel ends
               
               sidebarPanel( # Our "image" panel
                 width = 3, # spacing, allowing for the 3 side-by-side segments
                 h4("How we're feeling:"),
                 img(src="https://dudewipes.com/cdn/shop/articles/gigachad.jpg?v=1667928905&width=1024",
                     height = 150,
                     width = 300), # Placeholder image for now. - should reach to lm fit
                 
               ), # sidebarPanel - image  ends
               
      ), # tabPanel - Forecasting  ends
      
      tabPanel("About", "This panel will be developed in time"),
  
    ) # navbarPage ends
    
  ) # fluidPage ends




# Define server logic required to plot the spending
server <- function(input, output) {

    output$spendPlot <- renderPlot({ # data from linear models module, should be fed in from user
      x <- seq(1, 30, 1)
      e <- rnorm(30, 0, 5)
      y <- 10 + 2 * x + e
      plot(x, y)
      f1 <- lm(y~x)
      abline(f1, col = 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

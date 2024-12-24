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
                 textInput("txt1", "Given Name:", ""),
                 textInput("txt2", "Surname:", ""),
                 
               ), # sidebarPanel - input ends
               
               mainPanel(
                 width = 6, # spacing, allowing for the 3 side-by-side segments
                 h1("This Paycheck:"),
                 verbatimTextOutput("txtout"),

               ), # mainPanel ends
               
               sidebarPanel( # Our "image" panelS
                 width = 3, # spacing, allowing for the 3 side-by-side segments
                 h4("How we're feeling:"),
                 img(src="https://dudewipes.com/cdn/shop/articles/gigachad.jpg?v=1667928905&width=1024",
                     height = 150,
                     width = 300), # Placeholder image for now.
                 
               ), # sidebarPanel - image  ends
               
      ), # tabPanel - Forecasting  ends
      
      tabPanel("About", "This panel will be developed in time"),
  
    ) # navbarPage ends
    
  ) # fluidPage ends




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

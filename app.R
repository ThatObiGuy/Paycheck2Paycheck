# Paycheck - 2 - paycheck
# Owen F. O'Connor

library(shiny)
library(shinythemes) # Adding themeing package, quick way to make app more appealing with minimal effort

# Define UI for application
ui <- fluidPage(theme = shinytheme("superhero"), # Implementation of shinythemes library called above - I like the superhero theme, found @ https://rstudio.github.io/shinythemes/
  
  navbarPage( # Allows us to use navigation bar at the top of the page, and structure in tabs
    title = "Paycheck2Paycheck", # Text displayed in the top left corner
    
    tabPanel("Forecasting", # This is the title of this panel, displayed in our navbar - the spending forecast happens here
             
             sidebarPanel( # Our "input" panel within the forecasting tab
               width = 3, # spacing, allowing for the 3 side-by-side segments
               
               tags$h3("Parameters:"), # Here we'll prompt for paycheck amount and days till next, these will help us construct the graph. We can use tags to access HTML from shiny
               numericInput("paycheck", "Paycheck amount", value = 0, min = 0, max = 5000), # Must be non-negative
               numericInput("rent", "rent contribution", value = 0, min = 0, max = 5000), # Must be non-negative
               numericInput("days", "Days till next paycheck", value = 0, min = 0, max = 365), # Also must be non-negative
               actionButton("buildGraphButton", "Build Graph", class = "btn-lg btn-success"), # Button 'locks-in' entered values. classes lg makes it large, and success makes it green
               
               tags$h3("Daily aggregate:"), # Here we'll take in data on spending/earning that will populate the graph
               numericInput("moneyMove", "Money Move", value = 0, min = -1000, max = 1000), # Money can move in or out of ones account.
               actionButton("moneyEarnButton", "EARN", class = "btn-lg btn-success"), # Buttons 'locks-in' entered value for a given day
               actionButton("moneySpendButton", "SPEND", class = "btn-lg btn-danger"), # A green EARN and red SPEND button make app more intuative
             ), # sidebarPanel - input ends
             
             mainPanel(
               width = 6, # Spacing, allowing for the 3 side-by-side segments
               h1("This Paycheck:"),
               
               plotOutput("spendPlot"), # Displays plot generated on server side.
             ), # mainPanel ends
             
             sidebarPanel( # Our "image" panel - Here we provide feedback through images
               width = 3, # spacing, allowing for the 3 side-by-side segments
               
               h4("How we're feeling:"),
               uiOutput("feelingImage"), # Using uiOutput allows image to react to lm's slope.
               
               h4("Advice:"),
               uiOutput("adviceText")
             ), # sidebarPanel - image  ends
             
             sidebarPanel( # Our "Upload/Download" panel - Here we add functionality to save and resume previous progress
               selectInput("menu", "Upload/Download session progress:", # Drop-down style menu makes page appear "less-busy"
                           choices = c("Select an option:", "Upload", "Download")),
               conditionalPanel( # Conditional panels appear when their 'choice' from above is selected
                 condition = "input.menu == 'Select an option:'",
               ),
               conditionalPanel(
                 condition = "input.menu == 'Upload'",
                 h4("Resume progress:"),
                 fileInput("upload", "Choose .rda File", # Use of .rda file allows for me to use less code, and makes sense to me, an r enjoyer.
                           multiple = FALSE, # Only makes sense to upload one file, app can only handle 1 session at a given time.
                           accept = c(".rda")), # Makes sure extension of file is .rda
               ), # fileInput gives us a button to chose file, upload progress from different session
               conditionalPanel(
                 condition = "input.menu == 'Download'",
                 h4("Save for later:"),
                 downloadButton("downloadData", "Download"), # title name for us to refer to from server side and label for button
               ), # fileInput gives us a button for download if user wants to come back to it later

             ), # sidebarPanel - download/upload  ends
             
    ), # tabPanel - Forecasting  ends
    
    tabPanel("How to use", # A description of how the app should be used
             
            h2("How to Use the App"),
            p("1. Enter your paycheck amount and the number of days until your next paycheck."),
            p("2. Click 'Build Graph'."),
            p("3. Input your daily aggregate earnings/spending"),
            p("4. Use the advice and visual feedback to make more informed financial decisions ;)"),
            hr(),
            p("You can watch the video below as an example and guide."),
            div(style = "text-align: center;", 
                tags$iframe(width="560", height="315",
                            src="https://www.youtube.com/embed/OErMB4WYkjE?si=0hzi8FV5ioTW2dlI",
                            frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
                            allowfullscreen=NA)) # A quick video showing how app is to be used.
            
    ), # tabPanel - How to use ends
    
    
    tabPanel("About", # Some information about the app
             
             h2("About Paycheck2Paycheck"), 
             p("A Shiny app to help people living \"Paycheck-to-Paycheck\" to see if they're spending too fast."),
             p("This is being developed as my application to the 2024/2025 Maynooth Data Science \"Shiny App Developement Competition\""),
             p("Intention is to have an easy-to-use webapp that will take in spending from a user on one side and populate a graph in the main section."),
             p("This graph will fit a line to the spending and through a series of informative & humours images indicate to the user if they're going to make it through to their next paycheck with any money."),
             hr(), # Horizontal rule
             
             h3("About the Developer"),
             p("Owen F. O'Connor is a third year MH207 data science student"),
             p("you can email @ owen.oconnor.2024@mumail.ie"),
             tags$a(href="https://mulife.ie/society/data-science", "Data science society"),
             br(),
             tags$a(href="https://www.instagram.com/that.obi.guy/", "Instagram"),
             br(),
             tags$a(href="https://www.linkedin.com/in/owen-f-o-connor-7565001b3/", "LinkedIn")
    )
             
  ) # navbarPage ends
                
) # fluidPage ends




# Define server logic required to plot the spending
server <- function(input, output) {
  
  # Reactive values to store inputs
  user_data <- reactiveValues(days = NULL,
                              paycheck = NULL,
                              rent = NULL,
                              moneyMoves = numeric(),
                              pressCounter = 0,
                              imageURL = "https://raw.githubusercontent.com/ThatObiGuy/Paycheck2Paycheck/refs/heads/main/ImagesResized/Waiting.png",
                              adviceText = "waiting on more data before providing advice")
  
  observeEvent(input$upload, { # observeEvents looks for user to upload file, at which point it can update resume previous session
    req(input$upload)
    load(input$upload$datapath)
    user_data$days <- user_data_list$days # Assigns the values from the lists (uploaded) to this session's values
    user_data$paycheck <- user_data_list$paycheck
    user_data$rent <- user_data_list$rent
    user_data$moneyMoves <- unlist(user_data_list$moneyMoves)
    user_data$pressCounter <- user_data_list$pressCounter
    user_data$imageURL <- user_data_list$imageURL
    user_data$adviceText <- user_data_list$adviceText
  })
  
  observeEvent(input$buildGraphButton, { # observeEvents looks for 'build graph' button presses, at which point it can update the stored values
    user_data$days <- input$days
    user_data$paycheck <- input$paycheck
    user_data$rent <- input$rent
    user_data$moneyMoves <- numeric() # Reset moneyMoves when building a new graph
    user_data$pressCounter <- 0 # Reset counter when building a new graph
    user_data$imageURL <- "https://raw.githubusercontent.com/ThatObiGuy/Paycheck2Paycheck/refs/heads/main/ImagesResized/Waiting.png"
    user_data$adviceText <- "waiting on more data before providing advice"
  })
  
  observeEvent(input$moneyEarnButton, { # Appends moneyMoves vector with new values, each ideally for consecutive days
    user_data$pressCounter <- user_data$pressCounter + 1
    user_data$moneyMoves[user_data$pressCounter] <- input$moneyMove
  })
  
  observeEvent(input$moneySpendButton, { # Appends moneyMoves vector with new values, each ideally for consecutive days
    user_data$pressCounter <- user_data$pressCounter + 1
    user_data$moneyMoves[user_data$pressCounter] <- (input$moneyMove * -1)
  })
  
  output$spendPlot <- renderPlot({
    
    validate(
      need(user_data$days > 0, "Please enter a valid number of days."),
      need(user_data$paycheck > 0, "Please enter a valid paycheck amount.")
    ) # Cannot build graph without these parameters
    
    x <- seq(1, user_data$days, 1)
    y <- numeric(length(x+1)) # Initialize y as a numeric vector of zeros ## Plus 1 to include day of paycheck recieved
    y[1] <- (user_data$paycheck - user_data$rent) # Balance begins as the amount of your paycheck less rent
    
    for (i in 2:length(x)) {
      if (i - 1 <= length(user_data$moneyMoves)) {
        y[i] <- y[i - 1] + user_data$moneyMoves[i - 1]
      } else {
        y[i] <- NA # no value for days before we've inputted their moneyMove - reveals points as they become relevant
      }
    }
    
    
    par(mar = c(5.1, 7, 4.1, 2.1)) # Modify margins of plot so we can have horizontal ylab
    
    plot(x, y, pch = 16,
         ylim = c(-50, (user_data$paycheck-user_data$rent) * 1.25), 
         xlab = "Days",
         ylab = " ") # ylab empty as we want it horizontal
    abline(h = 0,
           lwd = 2,
           lty = 2,
           col = "black")
    mtext("Balance",
          side = 2,
          line = 3,
          las = 1) # ylab title manually
    
    # Adding a line to the plot to project our end-of-month balance
    if (sum(!is.na(y)) > 1) { # If there's more than 1 non-NA point / at least two points
      fit <- lm(y ~ x) # we're fitting a linear model
      abline(fit,
             col = "red",
             lty = 5, # "line type" - 5 indicates long dashes
             lwd = 2) # and we're adding it to the plot
      
      
      # Update image based on the slope of the linear model
      if ((user_data$paycheck - user_data$rent) + user_data$days*coef(fit)[2] > (user_data$paycheck - user_data$rent)){
        user_data$imageURL <- "https://raw.githubusercontent.com/ThatObiGuy/Paycheck2Paycheck/refs/heads/main/ImagesResized/Swimming.png"
        user_data$adviceText <- "You don't need this app"
      } else if ((user_data$paycheck - user_data$rent) + user_data$days*coef(fit)[2] > 0) { # we expect to have some money at when we receive next paycheck
        user_data$imageURL <- "https://raw.githubusercontent.com/ThatObiGuy/Paycheck2Paycheck/refs/heads/main/ImagesResized/gigachad.png"
        user_data$adviceText <- "Keep it up, You've got MONEY!"
      } else if ((user_data$paycheck - user_data$rent) + user_data$days*coef(fit)[2] == 0) { # we expect to have no debt when we receive next paycheck
        user_data$imageURL <- "https://raw.githubusercontent.com/ThatObiGuy/Paycheck2Paycheck/refs/heads/main/ImagesResized/spiderman.png"
        user_data$adviceText <- "Surviving another month!"
      } else if ((user_data$paycheck - user_data$rent) + user_data$days*coef(fit)[2] > -50){ # we expect to have less than 50 euros debt when we receive next paycheck
        user_data$imageURL <- "https://raw.githubusercontent.com/ThatObiGuy/Paycheck2Paycheck/refs/heads/main/ImagesResized/UnsettledTom.png"
        user_data$adviceText <- "It's no so bad, you've got savings... or friends?"
      } else { # we expect to have more than or equal to 50 euros debt when we receive next paycheck
        user_data$imageURL <- "https://raw.githubusercontent.com/ThatObiGuy/Paycheck2Paycheck/refs/heads/main/ImagesResized/TwoThousandYardStare.png"
        user_data$adviceText <- "good luck soldier"
      }
      
    }
  })
  
  output$feelingImage <- renderUI({ # renderUI allows us to work with uiOutput above
    img(src = user_data$imageURL)
  })
  
  output$adviceText <- renderUI({
    p(user_data$adviceText)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { # Filename is based on time of save
      paste("Paycheck2Paycheck-Save-", Sys.Date(), ".rda", sep="")
    },
    content = function(file) { # .rda structure allows us to be very efficient
      user_data_list <- reactiveValuesToList(user_data)
      save(user_data_list, file = file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

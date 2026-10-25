#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Dark/Light mode
    tags$head(
      tags$style(HTML("
          body {
              background-color: white;
              color: black;
              transition: background-color 0.3s, color 0.3s;
          }
  
          body.dark-mode {
              background-color: #222222;
              color: white;
          }
  
          body.dark-mode .well {
              background-color: #333333;
              border-color: #555555;
          }
  
          body.dark-mode .form-control {
              background-color: #444444;
              color: white;
              border-color: #666666;
          }
      ")),
      
      tags$script(HTML("
          Shiny.addCustomMessageHandler('darkMode', function(enabled) {
              document.body.classList.toggle('dark-mode', enabled);
          });
      "))
    ),
  

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    sidebarLayout(
        sidebarPanel(
          
            # Widget 1: Sidebar with a slider input for number of bins
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            # Widget 2: Selecting the variable of interest
            selectInput("variable",
                        "Choose a variable:",
                        choices = c("Waiting time" = 2,
                                    "Eruption duration" = 1)),
        
            # Widget 3: Checkbox input for light/dark mode
            checkboxInput("color",
                          "Dark Mode",
                          value = FALSE),
            
            # Widget 4: Radio buttons for histogram v. boxplot
            radioButtons("plot_type",
                         "Choose plot type:",
                         choices = c("Histogram", "Boxplot"),
                         selected = "Histogram"),
            
            # Widget 5: Action button to reset to default
            actionButton("reset",
                         "Reset settings")
        ),
            

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # reset the inputs
    observeEvent(input$reset, {
      updateSliderInput(session, "bins", value = 30)
      updateSelectInput(session, "variable", selected = 2)
      updateCheckboxInput(session, "color", value = FALSE)
      updateRadioButtons(session, "plot_type",
                         selected = "Histogram")
    })
    
    # Check and switch to dark/light mode
    observe({
      session$sendCustomMessage("darkMode", input$color)
    })
  
    output$distPlot <- renderPlot({
      
        # set plot background and text colors for dark/light mode
        if (input$color) {
          par(bg = "#222222",
              fg = "white",
              col.axis = "white",
              col.lab = "white",
              col.main = "white")
        } else {
          par(bg = "white",
              fg = "black",
              col.axis = "black",
              col.lab = "black",
              col.main = "black")
        }
      
        # generate bins based on input$bins from ui.R
      
        # select a variable
        x <- faithful[, as.numeric(input$variable)]
        
        
        # draw the selected plot
        if (input$plot_type == "Histogram") {
          bins <- seq(min(x), max(x), length.out = input$bins + 1)
          
          # draw the histogram with the specified number of bins
          hist(x, breaks = bins, col = 'darkgray', border = 'white',
              xlab = if (input$variable == "2")
                "Waiting time (in mins)" else
                  "Eruption duration (in mins)",
              main = 'Histogram of Old Faithful Data',
              col.main = if (input$color) "white" else "black",
              col.lab = if (input$color) "white" else "black",
              col.axis = if (input$color) "white" else "black")
        } else {
          
          boxplot(x,
                col = if (input$color) "darkgray" else "white",
                border = if (input$color) "white" else "black",
                xlab = "Old Faithful",
                ylab = if (input$variable == "2")
                  "Waiting time (in mins)" else
                    "Eruption duration (in mins)",
                main = "Boxplot of Old Faithful Data",
                col.main = if (input$color) "white" else "black",
                col.lab = if (input$color) "white" else "black",
                col.axis = if (input$color) "white" else "black")
                  
        }
        

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

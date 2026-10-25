#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(macleish)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weather Time Series Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "dateRange",
                label = "Select Date Range:",
                min = as.Date(min(whately_2015$when)),
                max = as.Date(max(whately_2015$when)),
                value = c(as.Date(min(whately_2015$when)), as.Date(max(whately_2015$when))),
                timeFormat = "%Y-%m-%d"
            ),
            radioButtons(
                "station",
                "Select Weather Station:",
                choices = c("Whately", "Orchard"),
                selected = "Whately"
            ),
            actionButton(
                "reset",
                "Reset settings"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Reset the inputs
    observeEvent(input$reset, {
        updateSliderInput(session, "dateRange", value = c(as.Date(min(whately_2015$when)), as.Date(max(whately_2015$when))))
        updateRadioButtons(session, "station", selected = "Whately")
    })    
    
    output$distPlot <- renderPlot({
      
        # Filter station
        station_data <- if (input$station == "Whately") {
            whately_2015
        } else {
            orchard_2015
        }
        
        # Filter to dates
        station_data |> filter(when >= input$dateRange[1], when <= input$dateRange[2])
        
        
        station_data |> ggplot(aes(x = when, y = temperature)) +
            geom_line(size = 0.5) +
            coord_cartesian(
                xlim = as.POSIXct(
                    c(input$dateRange[1], input$dateRange[2]),
                    tz = "UTC"
                )
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

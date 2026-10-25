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
            selectInput(
                "variable",
                "Select Weather Feature:",
                choices = c("temperature", "wind_speed", "wind_dir", "rel_humidity", "pressure", "rainfall"),
                selected = "temperature"
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

server <- function(input, output, session) {

    # Reset the inputs
    observeEvent(input$reset, {
        updateSliderInput(session, "dateRange", value = c(as.Date(min(whately_2015$when)), as.Date(max(whately_2015$when))))
        updateRadioButtons(session, "station", selected = "Whately")
    })
  
    # Change min and max of date sliders, and clamp if outside of range
    # Only a safety precaution
    observeEvent(input$station, {
        # Filter station
        station_data <- if (input$station == "Whately") {
            whately_2015
        } else {
            orchard_2015
        }
        
        # Determine the new station's date limits
        station_min <- as.Date(min(station_data$when, na.rm = TRUE))
        station_max <- as.Date(max(station_data$when, na.rm = TRUE))
        
        # Get the currently selected dates
        current_range <- input$dateRange
        
        # Clamp the selected dates to the new station's limits
        clamp_date <- function(date, lower, upper) {
            if (date < lower) {
                lower
            } else if (date > upper) {
                upper
            } else {
                date
            }
        }
        
        new_start <- clamp_date(current_range[1], station_min, station_max)
        new_end   <- clamp_date(current_range[2], station_min, station_max)
        
        # Update the slider limits and selected range
        updateSliderInput(
            session,
            inputId = "dateRange",
            min = station_min,
            max = station_max,
            value = c(new_start, new_end)
        )
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
        
        
        station_data |> ggplot(aes(x = when, y = .data[[input$variable]])) +
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

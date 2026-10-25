#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(palmerpenguins)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("sex",
                              "Penguin Sex:",
                               choices = c("male", "female"),
                               selected = c("male", "female")
            
            ),
            selectInput("species",
                        "Penguin Species:",
                        choices = unique(penguins$species),
                        multiple = TRUE,
                        selected = unique(penguins$species)
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

    output$distPlot <- renderPlot({
        # Validate that a gender is selected.
        validate(
            need(
                length(input$sex) > 0,
                "Please select at least one sex."
            )
        )
      
        # Validate that a species is selected.
        validate(
            need(
                length(input$species) > 0,
                "Please select at least one species."
            )
        )
      
        # Initialize
        penguins_filtered <- penguins
        
        # Filter sex, if sex argument > 1, don't filter (NA values for sex included)
        if (length(input$sex) == 1) {
            penguins_filtered <- penguins_filtered |>
                filter(sex == input$sex)
        }
        # Filter species
        penguins_filtered <- penguins_filtered |>
            filter(species == input$species)
        
        ggplot(penguins_filtered,
            aes(x = flipper_length_mm)) +
            geom_histogram()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

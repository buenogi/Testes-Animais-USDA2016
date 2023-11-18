#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
            mainPanel( 
                    leafletOutput("map")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -99, lat = 37.5, zoom = 4)
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

# Load packages
library(shiny)
library(leaflet)

# Import Data
data <- read.csv("shiny_data.csv")

# UI
ui <- fluidPage(
    p(),
    leafletOutput("mymap"),
    actionButton("recalc", "New points")
)

# SERVER
server <- function(input, output, session) {
    
    # Generate Leaflet map with points from "Data"
    output$mymap <- renderLeaflet({
        leaflet(data) %>%
            addTiles() %>%
            setView(median(data$longitude), median(data$latitude), zoom = 8) %>%
            addCircles(~longitude, 
                       ~latitude, 
                       popup = data$listing_name,
                       weight = 1, 
                       radius= 200,
                       color= "blue", 
                       fillOpacity = 0.3)
    })
}

# CALL SHINY
shinyApp(ui, server)

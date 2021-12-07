# Load packages
library(shiny)
library(leaflet)

# Import Data
data <- read.csv("shiny_data.csv")

# UI
ui <- fluidPage(
    p(),
    # selectInput(inputId = "locale_choice",
    #              label = "Choose Locale",
    #              choices = locale_choices,
    #              selected = "Austin"),
    leafletOutput("mymap"),
    actionButton("recalc", "New points")
)

# SERVER
server <- function(input, output, session) {
    
    # filtered_points <- reactive({
    #     new_points <- filterData() %>%
    #         filter(data, locale %in% input$locale_choice)
    # })
    
    output$mymap <- renderLeaflet({
        leaflet(data) %>%
            addTiles() %>%
            setView(median(data$longitude), median(data$latitude), zoom = 8) %>%
            addCircles(~longitude, 
                       ~latitude, 
                       popup = data$listing_name,
                       weight = 1, 
                       radius=200,
                       color="blue", 
                       fillOpacity = 0.3)
    })
}

# CALL SHINY
shinyApp(ui, server)

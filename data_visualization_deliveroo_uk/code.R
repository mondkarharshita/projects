install.packages("ggmap")
#install.packages("geosphere")
# load required packages
library(shiny)
library(dplyr)
library(geosphere)
library(ggmap)

# Define UI for Shiny app
ui <- fluidPage(
  
  # App title
  titlePanel("Deliveroo Restaurant Data"),
  
  # Sidebar with input for delivery address
  sidebarLayout(
    sidebarPanel(
      textInput("delivery_address", "Enter delivery address", value = "London, UK"),
      actionButton("submit", "Submit")
    ),
    
    # Main panel with map output
    mainPanel(
      plotOutput("map")
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  
  # Load the data
  deliveroo_data <- read.csv("deliveroo_data.csv", stringsAsFactors = FALSE)
  
  # Geocode the delivery and restaurant addresses
  delivery_coords <- deliveroo_data %>% 
    select(delivery_address) %>% 
    mutate(coords = geocode(delivery_address)) %>% 
    select(-delivery_address)
  
  restaurant_coords <- deliveroo_data %>% 
    select(restaurant_address) %>% 
    mutate(coords = geocode(restaurant_address)) %>% 
    select(-restaurant_address)
  
  # Define a reactive function to update the map based on input
  delivery_map <- reactive({
    
    # Geocode the input delivery address
    input_coords <- geocode(input$delivery_address)
    
    # Calculate the distance and bearing between the input and all restaurant locations
    delivery_restaurant <- data.frame(
      restaurant_coords = restaurant_coords$coords,
      distance = distGeo(input_coords, restaurant_coords$coords),
      bearing = bearing(input_coords, restaurant_coords$coords)
    )
    
    # Plot the delivery and restaurant locations on a map
    map <- get_map(location = mean(delivery_coords$coords), zoom = 12, maptype = "roadmap")
    map_gg <- ggmap(map) +
      geom_point(data = delivery_coords, aes(x = lon, y = lat), color = "red", size = 3) +
      geom_point(data = restaurant_coords, aes(x = lon, y = lat), color = "blue", size = 3)
    
    # Add the input delivery location and a line indicating the distance and bearing to the nearest restaurant
    nearest_restaurant <- delivery_restaurant %>% 
      slice(which.min(distance)) %>% 
      select(restaurant_coords, distance, bearing)
    
    map_gg + 
      geom_point(data = data.frame(coords = input_coords), aes(x = lon, y = lat), color = "green", size = 3) +
      geom_segment(aes(x = input_coords[1], y = input_coords[2], 
                       xend = nearest_restaurant$restaurant_coords[1], yend = nearest_restaurant$restaurant_coords[2]), 
                   color = "black", size = 1.5, arrow = arrow(length = unit(0.3, "cm"))) +
      annotate("text", x = mean(c(input_coords[1], nearest_restaurant$restaurant_coords[1])), 
               y = mean(c(input_coords[2], nearest_restaurant$restaurant_coords[2])), 
               label = paste0("Distance: ", round(nearest_restaurant$distance/1000, 2), " km, Bearing: ", round(nearest_restaurant$bearing, 2), " degrees"), 
               size = 5, hjust = 0, vjust = -1
               
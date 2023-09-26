library(shiny)
library(dplyr)
library(ggplot2)

library(scales)

library(plotly)
library(leaflet)
library(tidyr)
library(shinyWidgets)

library(wordcloud2)
library(tm)
library(stringr)

restaurants <- read.csv("deliveroo.csv")

ui <- fluidPage(
  titlePanel("Restaurant Dashboard"),
  mainPanel(
    tabsetPanel(
      tabPanel("Dashboard",
               fluidRow(
                 column(width = 3,
                        h4("Data Summary"),
                        tableOutput("summary")),
                 column(width = 9,
                        h4("Top Restaurants"),
                        plotlyOutput("top_restaurants", height = "400px")),
               ),
               fluidRow(
                 column(width = 6,
                        h4("Delivery Pie Chart"),
                        plotOutput("delivery_pie")),
                 column(width = 6,
                        h4("Location Pie Chart"),
                        plotOutput("location_pie"))
               )
               
      ),
      tabPanel("Scatter Plot",
               fluidRow(
                 h4("Delivery Time vs Distance"),
                 plotlyOutput("delivery_distance")
               ),
               fluidRow(
                 h4("Delivery Fee vs Distance"),
                 plotlyOutput("deliveryfee_distance")
               )
               
      ),
      tabPanel("Bubble Plot",
               h4("Deliveroo Restaurant Analysis"),
               plotlyOutput("bubble_plot", height = "600px")
      ),
      tabPanel("Word cloud",
               h4("Cuisine Word Cloud"),
               wordcloud2Output("wordcloud", height = "600px")
      ),
      tabPanel("Map",
               leafletOutput("map", height = "800px"))
    )
  ),
  
  sidebarPanel(
    selectInput("location", "Select Location area", 
                choices = c("All", unique(restaurants$searched_category))),
    selectInput("zipcode", "Select zipcode of area", 
                choices = c("All", unique(restaurants$searched_zipcode))),
    textInput(inputId = "food", label = "Type a cuisine name:", value = ""),
    sliderInput("rating", "Filter by Rating", 
                min = 0, max = 5, value = c(0, 5), step = 0.5),
    checkboxInput("delivery", "Free delivery", value = FALSE),
    checkboxInput("deliveroo", "Only on Deliveroo", value = FALSE),
    sidebarClass = "sidebar"
  )
)

# Define server
server <- function(input, output, session) {
  
  # Modify data
  restaurants$delivery_fee[is.na(restaurants$delivery_fee)] <- 0
  restaurants$delivery_fee <- as.numeric(restaurants$delivery_fee)
  
  restaurants$delivery <- ifelse(restaurants$delivery_fee == 0, "Free", "Paid")
  restaurants$only_on_deliveroo <- as.logical(restaurants$only_on_deliveroo)
  
  restaurants$review_count <- ifelse(restaurants$review_count == "500+", 500, as.integer(restaurants$review_count))
  
  restaurants$delivery_time[is.na(restaurants$delivery_time)] <- 0
  restaurants$delivery_time <- as.numeric(restaurants$delivery_time)
  restaurants$review_rating <- as.numeric(restaurants$review_rating)
  
  # nlp
  restaurants$cuisine <- ifelse(grepl("Serves", restaurants$description), 
                                gsub("Serves | and", "", regmatches(restaurants$description, regexpr("(?<=Serves ).*?(?=\\.)", restaurants$description, perl = TRUE))),
                                NA)  
  
  
  # Create a new column for the delivery fee range
  restaurants$delivery_fee_range <- cut(restaurants$delivery_fee, breaks = seq(0, 5, 1))
  
  
  # Filter the data based on user input
  filtered_data <- reactive({
    data <- restaurants
    
    # location area
    if (input$location != "All") {
      data <- filter(data, searched_category == input$location)
      
      # Update zipcode choices based on selected location
      updateSelectizeInput(session, "zipcode", 
                           choices = c("All", unique(data$searched_zipcode)),
                           selected = input$zipcode)
      
      # Update cuisine choices based on selected zipcode
      if (input$zipcode != "All") {
        data <- filter(data, searched_zipcode == input$zipcode)}
    } else {
      # Update zipcode choices for all locations
      updateSelectizeInput(session, "zipcode", 
                           choices = c("All", unique(data$searched_zipcode)),
                           selected = input$zipcode)
      
      # Update cuisine choices for all locations
      if (input$zipcode != "All") {
        data <- filter(data, searched_zipcode == input$zipcode)}
    }
    data <-  subset(data, grepl(tolower(input$food), tolower(cuisine)))
    
    data <- filter(data, review_rating >= input$rating[1] & review_rating <= input$rating[2])
    if (input$delivery) {
      data <- filter(data, delivery == "Free")
    }
    if (input$deliveroo) {
      data <- filter(data, only_on_deliveroo == TRUE)
    }
    data
  })
  
  
  
  
  # Summary table
  output$summary <- renderTable({
    data <- filtered_data()
    summary_table <- data %>%
      summarise(
        n_restaurants = as.integer(n()),
        n_categories = as.integer(n_distinct(searched_category)),
        n_cuisines = as.integer(n_distinct(cuisine)),
        avg_delivery_time = mean(delivery_time, na.rm=TRUE),
        avg_delivery_fee = mean(delivery_fee),
        avg_review_rating = mean(review_rating),
        avg_n_review = mean(review_count)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
    summary_table
  })
  
  
  # Top 10 restaurants bar plot
  output$top_restaurants <- renderPlotly({
    data <- filtered_data()
    
    top_restaurants <- data %>%
      arrange(desc(review_rating)) %>%
      head(10) %>%
      ggplot(aes(reorder(loc_name, review_rating), review_rating, fill = as.factor(review_rating), group = as.factor(review_rating))) +
      geom_col() +
      xlab("Restaurant") +
      ylab("Average Rating") +
      ggtitle("Top 10 Restaurants") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_fill_brewer(palette = "Set3") # add color palette
    
    ggplotly(top_restaurants)
  })
  
  
  
  # Map 
  output$map <- renderLeaflet({
    data <- filtered_data()
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(lng = ~searched_lng, lat = ~searched_lat, 
                 popup = paste("Restaurant Name: ", data$loc_name,
                               "<br>Rating: ", data$review_rating,
                               "<br>Reviews: ", data$review_count),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 50))
    
    ## Below code can be added if we have exact locations of the restaurants
      # addCircleMarkers(lng = ~searched_lng, lat = ~searched_lat,
      #                  radius = 5, 
      #                  color = ifelse(data$review_rating >= 4, "green", ifelse(data$review_rating >= 3, "orange", "red")),
      #                  fillOpacity = 0.8,
      #                  popup = paste("Rating: ", data$review_rating)) 
  })
  
  
  
  # delivery pie chart
  output$delivery_pie <- renderPlot({
    delivery_data <- restaurants %>%
      group_by(delivery) %>%
      summarize(count = n())
    
    ggplot(delivery_data, aes(x = "", y = count, fill = delivery)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = paste0(round((count/sum(count))*100), "%")), position = position_stack(vjust = 0.5))
  })
  
  
  # location pie chart
  output$location_pie <- renderPlot({
    location_data <- restaurants %>%
      group_by(searched_category) %>%
      summarize(count = n())
    
    ggplot(location_data, aes(x = "", y = count, fill = searched_category)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(x=1.6, label = paste0(round((count/sum(count))*100), "%")), position = position_stack(vjust = 0.5))
  })
  
  # scatter plot
  output$delivery_distance <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = distance, y = delivery_time)) +
      geom_point(aes(color = review_rating)) +
      xlab("Distance (km)") +
      ylab("Delivery Time (mins)") +
      ggtitle("Delivery Time vs Distance") +
      scale_color_gradient(low = "red", high = "green", guide = "colorbar")
    ggplotly(p)
  })
  
  # scatter plot
  output$deliveryfee_distance <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = distance, y = delivery_fee)) +
      geom_point(aes(color = review_rating)) +
      xlab("Distance (km)") +
      ylab("Delivery Fee (Pounds)") +
      ggtitle("Delivery Fee vs Distance") +
      scale_color_gradient(low = "red", high = "green", guide = "colorbar")
    ggplotly(p)
  })
  
  # word cloud
  output$wordcloud <- renderWordcloud2({
    data <- filtered_data()
    
    # Create a corpus from the description column
    corpus <- Corpus(VectorSource(data$cuisine))
    
    # Clean the corpus
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    # Create a document term matrix
    dtm <- DocumentTermMatrix(corpus)
    
    # Compute the word frequencies
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    
    # Create a word cloud using wordcloud2
    wordcloud2(data = data.frame(word = names(freq), freq = freq),
               size = 0.8,
               color = "random-dark",
               backgroundColor = "#F0F8FF")
  })
  # Bubble plot
  output$bubble_plot <- renderPlotly({
    data <- filtered_data()
    # Filter out rows with NA values in delivery_fee_range
    data <- subset(data, !is.na(delivery_fee_range))
    
    p <- ggplot(data, aes(x = review_count, y = delivery_time, size = delivery_fee, color = review_rating)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(3, 10)) +
      scale_color_gradient(low = "red", high = "green", guide = "colorbar") +
      labs(title = "Deliveroo Restaurant Analysis",
           x = "Review Count",
           y = "Delivery Time (minutes)",
           size = "Delivery Fee",
           color = "Review Rating") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom") +
      facet_wrap(~delivery_fee_range, nrow = 2) +
      ylim(0, 100) +
      geom_text(aes(label = loc_name), size = 3, vjust = -1)
    ggplotly(p)
  })
  
  
}


shinyApp(ui, server)


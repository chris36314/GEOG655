library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(sf)

# Load permit data
# Assuming the Building_Permits_2.csv file is in the same directory as your app files
permits <- read.csv("Building_Permits_2.csv")
neighborhoods <- st_read("geo_export_18d7ae09-aaae-4880-9c28-20165b1e9696.shp")

# Convert ISSUE_DATE to Date format
permits$ISSUE_DATE <- as.Date(permits$ISSUE_DATE, format = "%m/%d/%Y")  # Adjust format if needed

cleantable <- permits %>%
  select(
    PermitID = PERMIT.,
    Permit_Type = PERMIT_TYPE,
    Date = ISSUE_DATE,
    Street = STREET_NAME,
    StreetDirection = STREET.DIRECTION,
    StreetNumber = STREET_NUMBER,
    StreetSuffix = SUFFIX,
    Description = WORK_DESCRIPTION,
    PermitFee = SUBTOTAL_PAID,
    Latitude = LATITUDE,
    Longitude = LONGITUDE,
    pri_neigh = COMMUNITY_AREA
  )

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesFINAL.css")
  ),
  navbarPage("Permit Explorer", id="nav",
             
             tabPanel("Interactive Map",
                      leafletOutput("map"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    h2("Permit Explorer"),
                                    
                                    selectInput("permit_type", "Permit Type", c("All", unique(cleantable$Permit_Type))),
                                    fluidRow(
                                      column(6,
                                             dateInput("start_date", "Start Year", value = "2022-01-01")
                                      ),
                                      column(6,
                                             dateInput("end_date", "End Year", value = "2023-12-31")
                                      )
                                    ),
                                    hr(),
                                    numericInput("minPermitFee", "Min Permit Fee", min = 0, max = 100000, value = 0),
                                    numericInput("maxPermitFee", "Max Permit Fee", min = 0, max = 100000, value = 100000)
                      )
             ),
             
             tabPanel("Data Explorer",
                      DT::dataTableOutput("permitTable"),
                      plotOutput("barPlot"),
                      verbatimTextOutput("regression_result"),  # Regression output
                      verbatimTextOutput("anova_neighborhood"),  # ANOVA result
                      verbatimTextOutput("ancova_neighborhood_worktype")  # ANCOVA result
             )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Downsample permits
  downsampled_permits <- reactive({
    set.seed(42) # Set seed for reproducibility
    sampled_indices <- sample(1:nrow(cleantable), size = floor(0.001 * nrow(cleantable))) # Sample 0.1% of the data
    downsampled_data <- cleantable[sampled_indices, ]
    return(downsampled_data)
  })
  
  # Filter permits by selected criteria
  filtered_permits <- reactive({
    filtered <- downsampled_permits() %>%
      filter(Date >= input$start_date &
               Date <= input$end_date &
               PermitFee >= input$minPermitFee &
               PermitFee <= input$maxPermitFee)
    
    # Filter by Permit Type if selected
    if (input$permit_type != "All") {
      filtered <- filtered %>%
        filter(Permit_Type == input$permit_type)
    }
    
    return(filtered)
  })
  
  # ANOVA analysis for Neighborhood predicting Permit Fees
  output$anova_neighborhood <- renderPrint({
    # Perform ANOVA
    anova_result <- aov(PermitFee ~ pri_neigh, data = filtered_permits())
    # Print ANOVA summary
    summary(anova_result)
  })
  
  # ANCOVA analysis with Neighborhood and Work Type predicting Permit Fees
  output$ancova_neighborhood_worktype <- renderPrint({
    # Perform ANCOVA
    ancova_result <- lm(PermitFee ~ pri_neigh + Permit_Type, data = filtered_permits())
    # Print ANCOVA summary
    summary(ancova_result)
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%  # Modern gray basemap
      setView(lng = -87.6298, lat = 41.8781, zoom = 10) %>%  # Focus on Chicago
      addPolygons(data = neighborhoods, color = "blue", weight = 1, fillOpacity = 0.4, label = ~pri_neigh)
  })
  
  # Add permit markers to the map
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = filtered_permits(), lng = ~Longitude, lat = ~Latitude,
                 popup = paste("Permit ID:", filtered_permits()$PermitID,
                               "<br>Description:", filtered_permits()$Description,
                               "<br>Permit Fee:", filtered_permits()$PermitFee,
                               "<br>Work Type:", filtered_permits()$Permit_Type))
  })
  
  # Render data table
  output$permitTable <- DT::renderDataTable({
    filtered_permits()
  })
  
  # Render bar plot
  output$barPlot <- renderPlot({
    ggplot(data = filtered_permits(), aes(x = Permit_Type)) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = "Permits by Work Type", x = "Work Type", y = "Count") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

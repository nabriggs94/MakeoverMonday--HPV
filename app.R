# Install required packages, if not already installed, and load
if (!require("leaflet")) install.packages("leaflet")
library(leaflet)
if (!require("plotly")) install.packages("plotly")
library(plotly)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("rworldmap")) install.packages("rworldmap")
library("rworldmap")
if (!require("sf")) install.packages("sf")
library("sf")
if (!require("shiny")) install.packages("shiny")
library(shiny)

#Load the dataset from the Makeover Monday site on data.world
df <- read.csv("https://query.data.world/s/57gi57mo7ahtqu2mg6i36mhzbf3a52?dws=00000", 
               header=TRUE, stringsAsFactors=FALSE)

# Renamed the column representing vaccine coverage as the old name was super long
colnames(df)[4] <- "HPV_Coverage"

# Filter out rows with missing data
data_clean <- df %>% filter(!is.na(Code))

# Filter data for a given year
year_of_interest <- 2020  # Change this to the year you want to visualize
data_year <- data_clean %>% filter(Year == year_of_interest)

# Get world map data
world_map <- rworldmap::getMap(resolution = "low")
world_map_df <- st_as_sf(world_map)

###GGplot only Version

# Merge map data with HPV dataset
map_data <- world_map_df %>%
  left_join(data_year, by = c("ISO_A3" = "Code"))

# Define custom bins for HPV coverage for colour/shading
map_data <- map_data %>%
  mutate(
    Coverage_Category = case_when(
      is.na(HPV_Coverage) ~ "No Data",
      HPV_Coverage <= 16 ~ "0-16%",
      HPV_Coverage <= 43.36 ~ "16.01-43.36%",
      HPV_Coverage <= 68.75 ~ "43.37-68.75%",
      TRUE ~ "68.76%+")
 )

# Define a color-blind-friendly palette and texture
fill_colors <- c(
  "No Data" = "white",
  "0-16%" = "#A0764A",          # Dark Brown
  "16.01-43.36%" = "#FDE725",   # Golden Yellow
  "43.37-68.75%" = "#ADD8E6",   # Sky Blue
  "68.76%+" = "#364B9A"         # Dark Blue

)

# Plot the global map
ggplot(map_data) +
  geom_sf(aes(fill = Coverage_Category), color = "black", size = 0.1) +
  scale_fill_manual(
    values = fill_colors,
    name = "HPV Coverage",
    breaks = c("No Data", "0-16%", "16.01-43.36%", "43.37-68.75%", "68.76%+"),
    labels = c("No Data", "0-16%", "16.01-43.36%", "43.37-68.75%", "68.76%+")
  ) +
  labs(
    title = paste("Global HPV Vaccine Coverage in", year_of_interest),
    subtitle = "Shading represents the proportion who received the final dose (%)",
    caption = "Source: HPV Vaccine Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )
######## Shiny App version
ui <- fluidPage(
  titlePanel("Global HPV Vaccine Coverage"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "year", 
        "Select Year:", 
        choices = unique(data_clean$Year),
        selected = max(data_clean$Year)
      )
    ),
    
    mainPanel(
      leafletOutput("coverageMap", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # Reactive data for the selected year
  reactive_data <- reactive({
    data_clean %>%
      filter(Year == input$year)
  })
  
  # Merge map data with HPV dataset for the selected year
  map_data <- reactive({
    world_map_df %>%
      left_join(reactive_data(), by = c("ISO_A3" = "Code")) %>%
      mutate(
        Coverage_Category = case_when(
          is.na(HPV_Coverage) ~ "No Data",
          HPV_Coverage <= 16 ~ "0-16%",
          HPV_Coverage <= 43.36 ~ "16.01-43.36%",
          HPV_Coverage <= 68.75 ~ "43.37-68.75%",
          TRUE ~ "68.76%+"
        )
      )
  })
  
  # Create color palette function
  colorPalette <- reactive({
    colorFactor(
      palette = c(
        "white",     # No Data
        "#A0764A",   # Dark Brown (0-16%)
        "#FDE725",   # Golden Yellow (16.01-43.36%)
        "#ADD8E6",   # Sky Blue (43.37-68.75%)
        "#364B9A"    # Dark Blue (68.76%+)
      ),
      domain = c("No Data", "0-16%", "16.01-43.36%", "43.37-68.75%", "68.76%+"),
      ordered = TRUE
    )
  })
  
  # Render the global map
  output$coverageMap <- renderLeaflet({
    leaflet_data <- map_data()
    pal <- colorPalette()
    
    leaflet(leaflet_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Coverage_Category),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 2, color = "blue", fillOpacity = 1),
        label = ~paste0(
          "<strong>", ADMIN, "</strong><br>",
          "Coverage: ", ifelse(is.na(HPV_Coverage), "No Data", paste0(HPV_Coverage, "%"))
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~Coverage_Category,
        opacity = 0.7,
        title = "HPV Coverage",
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)

library(shiny)
library(tidyverse)
library(janitor)
library(dplyr)
library(plotly)
economy <- read_csv("Global Economy Indicators.csv")
economy <- economy |> 
  clean_names()
economy <- economy |> 
  mutate(per_capita_gdp = gross_domestic_product_gdp/population)
medals <- read_csv("Olympic_Medal_Tally_History.csv")
#Filter for relevant economic data
medals <- filter(medals, year >= 1970)
#Filter for only Olympic years
economy <- filter(economy, year >= 1972 & year %% 2 == 0)
#Weighted medal total, Gold = 3, Silver = 2, Bronze = 1
medals <- medals |> 
  mutate(weighted_total = gold*3 + silver *2 + bronze)
renamed <- economy |> 
  mutate(country = ifelse(country == "Czechoslovakia (Former)", "Czechoslovakia",
                          ifelse(country == "D.P.R. of Korea", "Democratic People's Republic of Korea",
                                 ifelse(country == "Ethiopia (Former)", "Ethiopia",
                                        ifelse(country == "China, Hong Kong SAR", "Hong Kong, China",
                                               ifelse(country == "Iran (Islamic Republic of)", "Islamic Republic of Iran",
                                                      ifelse(country == "Saudi Arabia", "Kingdom of Saudi Arabia",
                                                             ifelse(country == "Former Netherlands Antilles", "Netherlands Antilles",
                                                                    ifelse(country == "China", "People's Republic of China",
                                                                           ifelse(country == "USSR (Former)", "Soviet Union",
                                                                                  ifelse(country == "USSR (Former)", "Soviet Union",
                                                                                         ifelse(country == "Bahamas", "The Bahamas",
                                                                                                ifelse(country == "United Kingdom", "Great Britain",
                                                                                                       ifelse(country == "Viet nam", "Vietnam",country)
                                                                                                )))))))
                                               )           
                                        )        
                                 )
                          )))
merged <- merge(renamed, medals, by = c("country", "year"))
ranking <- merged |> 
  group_by(year, edition) |> 
  mutate(gdp_rank = rank(-gross_domestic_product_gdp),
         medal_rank = rank(-total),
         weight_rank = rank(-weighted_total),
         pop_rank = rank(-population))
# Define the UI for the application
ui <- fluidPage(
  titlePanel("Interactive GDP Rank vs Medal Rank"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting country
      selectInput("country", "Select Country", 
                  choices = c("All Countries", unique(ranking$country)), 
                  selected = "All Countries"),
      
      # Slider for selecting the year
      selectInput("edition", "Select Olympic Edition", 
                  choices = c("All Editions", sort(unique(ranking$edition))), 
                  selected = "All Editions")
    ),
    
    mainPanel(
      plotlyOutput("interactive_plot")
    )
  )
)

# Define the server logic for the application
server <- function(input, output) {
  
  # Reactive expression to filter the data based on user input
  filtered_data <- reactive({
    if (input$country == "All Countries") {
      country_data <- ranking
    } else {
      country_data <- ranking %>% filter(country == input$country)
    }
    
    # If "All Editions" is selected, use all editions; otherwise, filter by selected edition
    if (input$edition == "All Editions") {
      filtered <- country_data
    } else {
      filtered <- country_data %>% filter(edition == input$edition)
    }
    return(filtered)
  })
  
  # Generate the plot
  output$interactive_plot <- renderPlotly({
    plot_data <- filtered_data()
    
    # Create the plot with ggplot
    p <- ggplot(plot_data, aes(x = gdp_rank, y = medal_rank)) +
      geom_point(color = "blue", aes(text = paste("Country:", country, 
                                                  "<br>GDP Rank:", gdp_rank, 
                                                  "<br>Medal Rank:", medal_rank,
                                                  "<br>Edition:", edition))) +
      geom_smooth(method = "loess", color = "red", se = FALSE) +
      labs(title = "GDP Rank vs Medal Rank",
           x = "GDP Rank (Wealthier → Lower)",
           y = "Medal Rank (More Medals → Lower)") +
      theme_minimal()
    
    # Convert to plotly for interactivity
    ggplotly(p, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
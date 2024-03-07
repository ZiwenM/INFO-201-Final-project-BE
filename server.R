library(ggplot2)
library(plotly)
library(dplyr)

# Source file with dataframe
source("data/education.csv")
source("data/unemployment.csv")

# Source with analysis
source("analysis/education_impact_analysis.R")
source("analysis/education_income_area_impact_analysis.R")
source("analysis/education_rural_urban.R")
# Server function
server <- function(input, output) {
  
  # Simulated data frame for illustrating impacts
  education_impact_df <- reactive({
    data.frame(
      Year = rep(2000:2019, each = 4),
      Education_Level = rep(c("Less than High School", "High School Graduate", 
                              "Some College or Associate's Degree", "Bachelor's Degree or Higher"), times = 20),
      Area_Type = rep(c("City", "Rural"), each = 40),
      Impact_on_Median_Income = runif(80, 10000, 50000), 
      Impact_on_Education_Level = runif(80, 1, 10)
    )
  })
  
  # Process the education data for distribution plots
  education_data_long <- reactive({
    education %>% 
      gather(key = "Education_Level", value = "Percent", starts_with("Percent of adults")) %>%
      separate(Education_Level, into = c("Education_Level", "Year"), sep = ", ") %>%
      filter(!is.na(Percent)) %>%
      mutate(Percent = as.numeric(Percent),
             Education_Level = gsub("Percent of adults with ", "", Education_Level)) # Simplifying education level names
  })
  
  # Plot for Urban Areas
  output$plotUrban <- renderPlot({
    urban_data <- education_data_long() %>%
      filter(`City/Suburb/Town/Rural 2013` %in% c("City", "Suburb", "Town"))
    
    ggplot(urban_data, aes(x = Education_Level, y = Percent, fill = Education_Level)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribution of Education Levels in Urban Areas",
           y = "Percentage", x = "Education Level") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot for Rural Areas
  output$plotRural <- renderPlot({
    rural_data <- education_data_long() %>%
      filter(`City/Suburb/Town/Rural 2013` == "Rural")
    
    ggplot(rural_data, aes(x = Education_Level, y = Percent, fill = Education_Level)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribution of Education Levels in Rural Areas",
           y = "Percentage", x = "Education Level") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the selected plot based on user input
  output$impactPlot <- renderPlot({
    filtered_df <- education_impact_df() %>%
      filter(Area_Type %in% input$areaType)
    
    if(input$chartType == "Impact on Median Income") {
      ggplot(filtered_df, aes(x = Year, y = Impact_on_Median_Income, color = Education_Level)) +
        geom_line() +
        labs(title = "Impact of Education on Median Household Income Over Time",
             y = "Impact on Median Household Income", x = "Year") +
        theme_minimal()
    } else if(input$chartType == "Impact on Education Level") {
      ggplot(filtered_df, aes(x = Year, y = Impact_on_Education_Level, color = Education_Level)) +
        geom_line() +
        labs(title = "Changes in Education Level Over Time",
             y = "Change in Education Level", x = "Year") +
        theme_minimal()
    }
  })
  }
  


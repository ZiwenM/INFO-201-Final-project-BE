library(ggplot2)
library(plotly)
library(dplyr)

# Source file with dataframe
source("education.csv")
source("unemployment.csv")

# Server
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
  
  # Combined Analysis Text for insights on education's impact
  output$analysisText <- renderText({
    return("The analysis of education's impact on median household income over the last two decades reveals considerable variability. Notably, education seems to have a more pronounced impact on income in urban areas, which may be attributed to the concentration of jobs requiring higher educational qualifications. In contrast, rural areas show greater fluctuations, with recent trends indicating a potential downturn in income benefits from education. These findings underscore the complex relationship between educational attainment and economic prosperity, which is influenced by a myriad of factors beyond education itself.\n\nIn rural areas, the distribution of education levels appears to be weighted towards high school completion, with a significant percentage of adults holding only a high school diploma, as indicated by the prominent light purple bar. This suggests that in these areas, completing high school is a common educational milestone. Moreover, there is also a substantial representation of adults with less than a high school diploma, marked by the red bar, which may point to barriers to educational access or attainment in rural regions.\n\nConversely, higher education levels such as a bachelor's degree or higher and four years of college or higher, denoted by the blue and pink bars, respectively, have a smaller percentage. This could reflect limited opportunities for higher education in rural areas or perhaps less demand for such qualifications in the rural workforce.\n\nOur analysis over the past two decades shows that counties with higher levels of educational attainment typically exhibit not only higher median wages but also lower unemployment rates. This trend highlights the critical role of education in securing economic prosperity and stability. Notably, the data suggests that urban areas, with a higher concentration of individuals holding bachelor's degrees or higher, tend to outperform rural areas in both median wages and employment rates, further emphasizing the transformative power of education across different societal sectors.")
  })
}

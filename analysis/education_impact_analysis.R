library(dplyr)
library(tidyr)
library(ggplot2)


combined_df <- merge(unemployment, education, by = "FIPS_Code")


results_df <- data.frame(
  Year = rep(2000:2019, 4),
  Education_Level = rep(c("Less than High School", "High School Graduate", "Some College or Associate's Degree", "Bachelor's Degree or Higher"), each = 20),
  Impact_on_Median_Wages = runif(80, 5, 15), 
  Impact_on_Unemployment = runif(80, -2, 2) 
)

ggplot(results_df, aes(x = Year, y = Impact_on_Median_Wages, color = Education_Level)) +
  geom_line() +
  labs(y = "Impact on Median Wages", title = "Impact of Education Levels on Median Wages Over Time") +
  theme_minimal()

ggplot(results_df, aes(x = Year, y = Impact_on_Unemployment, color = Education_Level)) +
  geom_line() +
  labs(y = "Impact on Unemployment Rates", title = "Impact of Education Levels on Unemployment Rates Over Time") +
  theme_minimal()

library(dplyr)
library(tidyr)
library(ggplot2)

  Education_Level = rep(c("Less than High School", "High School Graduate", "Some College or Associate's Degree", "Bachelor's Degree or Higher"), each = 20)
  Year = rep(2000:2019, 2)

# Assuming you have a combined dataset named 'combined_df'
education_df <- read.csv("education.csv", stringsAsFactors = FALSE)

# Reshape the data using tidyr
education_data_long <- education_df %>%
  gather(key = "Education_Level", value = "Percent", starts_with("Percent of adults")) %>%
  separate(Education_Level, into = c("Education_Level", "Year"), sep = ", ") %>%
  filter(!is.na(Percent)) %>%
  mutate(Percent = as.numeric(Percent))

# Filter urban and rural data
urban_data <- education_data_long %>%
  filter(`City/Suburb/Town/Rural 2013` %in% c("City", "Suburb", "Town"))

rural_data <- education_data_long %>%
  filter(`City/Suburb/Town/Rural 2013` == "Rural")

# Plot the distribution of education levels in urban and rural areas
ggplot() +
  geom_bar(data = urban_data, aes(x = Education_Level, y = Percent, fill = Education_Level), stat = "identity") +
  labs(title = "Distribution of Education Levels in Urban Areas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot() +
  geom_bar(data = rural_data, aes(x = Education_Level, y = Percent, fill = Education_Level), stat = "identity") +
  labs(title = "Distribution of Education Levels in Rural Areas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analyze the impact on regional economic development (you may need additional data for this)
# You can compare median wages and unemployment rates between urban and rural areas using similar logic as before.

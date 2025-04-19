# Loading necessary libraries
library(tidyverse)
setwd("C:/Users/CHHANDAK/Documents/wage-disparity/datasets")

# Read the data set
wages <- read_csv("wages_by_education.csv")

# 1️⃣ Bar Chart: Men vs Women by Education Level (2022)
latest_data <- wages %>% filter(year == max(year))

gender_wage_data <- tibble(
  Education = c("Less than HS", "High School", "Some College", 
                "Bachelor's Degree", "Advanced Degree"),
  Men = c(latest_data$men_less_than_hs,
          latest_data$men_high_school,
          latest_data$men_some_college,
          latest_data$men_bachelors_degree,
          latest_data$men_advanced_degree),
  Women = c(latest_data$women_less_than_hs,
            latest_data$women_high_school,
            latest_data$women_some_college,
            latest_data$women_bachelors_degree,
            latest_data$women_advanced_degree)
)

gender_wage_long <- gender_wage_data %>%
  pivot_longer(cols = c(Men, Women), names_to = "Gender", values_to = "Wage")

ggplot(gender_wage_long, aes(x = Education, y = Wage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Wages by Gender and Education (2022)",
       x = "Education Level", y = "Average Hourly Wage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

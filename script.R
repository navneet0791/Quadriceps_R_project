# Loading necessary libraries
library(tidyverse)
library(here)

# Read the Dataset
wages <- read_csv(here("datasets", "wages_by_education.csv"))


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


# ⿢ Line Chart: Gender Wage Gap Over Time
wage_gap <- wages %>%
  transmute(
    year,
    `Less than HS` = men_less_than_hs - women_less_than_hs,
    `High School` = men_high_school - women_high_school,
    `Some College` = men_some_college - women_some_college,
    `Bachelor's Degree` = men_bachelors_degree - women_bachelors_degree,
    `Advanced Degree` = men_advanced_degree - women_advanced_degree
  )

wage_gap_long <- wage_gap %>%
  pivot_longer(cols = -year, names_to = "Education", values_to = "Wage_Gap")

ggplot(wage_gap_long, aes(x = year, y = Wage_Gap, color = Education)) +
  geom_line(linewidth = 1.2) +  geom_point(size = 2) + 
  labs(
    title = "Gender Wage Gap Over Time by Education Level",
    x = "Year",
    y = "Wage Gap (Men - Women, $/hr)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# Bar Plot: Gender Wage Gap as Percentage for the year 2022
gap_percent <- tibble(
  Education = factor(c("Less than HS", "High School", "Some College", 
                       "Bachelor's Degree", "Advanced Degree"),
                     levels = c("Less than HS", "High School", "Some College", 
                                "Bachelor's Degree", "Advanced Degree")),
  Gap_Percent = 100 * (c(latest_data$men_less_than_hs, latest_data$men_high_school, latest_data$men_some_college,
                         latest_data$men_bachelors_degree, latest_data$men_advanced_degree) -
                         c(latest_data$women_less_than_hs, latest_data$women_high_school, latest_data$women_some_college,
                           latest_data$women_bachelors_degree, latest_data$women_advanced_degree)) /
    c(latest_data$men_less_than_hs, latest_data$men_high_school, latest_data$men_some_college,
      latest_data$men_bachelors_degree, latest_data$men_advanced_degree)
)

ggplot(gap_percent, aes(x = Education, y = Gap_Percent, fill = Education)) +
  geom_bar(stat = "identity", width = 0.6) +  # narrower bars for spacing
  labs(title = "Gender Wage Gap (Percentage, 2022)",
       y = "Wage Gap (%)", x = "Education Level") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # add top space







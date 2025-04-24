# Loading necessary libraries
library(tidyverse)
library(here)

# Read the Dataset
wages <- read_csv(here("datasets", "wages_by_education.csv"))


# 1️⃣ Bar Chart: Men vs Women by Education Level (2022)
available_years <- unique(wages$year)
cat("Available years:", paste(available_years, collapse = ", "), "\n")
input_year <- as.integer(readline(prompt = "Enter the year you want to view data for: "))

# 🔍 Validate and Filter
if (!(input_year %in% available_years)) {
  stop("Invalid year entered. Please choose from the available years.")
}

latest_data <- wages %>% filter(year == input_year)

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
  labs(
    title = paste("Average Hourly Wages by Gender and Education Level (", input_year, ")", sep = ""),
    x = "Highest Educational Attainment",
    y = "Hourly Wage (in USD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.8))


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
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#  Linear Regression: Predicting Wages from Education & Gender

# Prepare the data for modeling
model_data <- gender_wage_long

# Convert Education and Gender to factors (important for modeling)
model_data <- model_data %>%
  mutate(
    Education = factor(Education, levels = c("Less than HS", "High School", "Some College", "Bachelor's Degree", "Advanced Degree")),
    Gender = factor(Gender)
  )

# Fit the linear model
wage_model <- lm(Wage ~ Education + Gender, data = model_data)

# Show model summary
summary(wage_model)

# Predicted wages from the model
model_data$Predicted_Wage <- predict(wage_model, model_data)

# Plot actual vs predicted wages with labels
ggplot(model_data, aes(x = Education, y = Wage, fill = Gender)) +
  # Bar plot for actual wages
  geom_col(position = position_dodge(width = 0.9), alpha = 0.7, width = 0.7) +
  
  # Actual wage labels on bars
  geom_text(aes(label = round(Wage, 0.5)),
            position = position_dodge(width = 0.9),
            vjust = -1.2, size = 2.3, color = "black") +
  
  # Predicted wage points
  geom_point(aes(y = Predicted_Wage, color = Gender),
             position = position_dodge(width = 0.9), shape = 21, size = 3, stroke = 1.2) +
  
  # Predicted wage labels near points
  geom_text(aes(y = Predicted_Wage, label = round(Predicted_Wage, 1), group = Gender),
            position = position_dodge(width = 0.9),
            vjust = 2.2, size = 2.3, color = "black")

# Plot labels and theme
labs(title = "Actual vs Predicted Wages by Education and Gender (2022)",
     subtitle = "Bars = Actual Wages | Dots = Predicted Wages from Linear Regression",
     y = "Wage ($/hr)", x = "Education Level") +
  scale_fill_manual(values = c("Men" = "#fca5a5", "Women" = "#7dd3fc")) +
  scale_color_manual(values = c("Men" = "#b91c1c", "Women" = "#0369a1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.subtitle = element_text(size = 10, face ="italic"))

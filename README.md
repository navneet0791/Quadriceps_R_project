# 📊 Gender Wage Analysis by Education Level

This R script performs an analysis of wage disparities between men and women across different levels of education using a dataset of average hourly wages over time.

## 📁 Dataset

The script expects a CSV file named `wages_by_education.csv` to be located in a `datasets/` directory at the root of the project. The dataset should contain wage data for men and women, broken down by education level and year.

### Expected Columns in the Dataset

- `year`
- `men_less_than_hs`
- `women_less_than_hs`
- `men_high_school`
- `women_high_school`
- `men_some_college`
- `women_some_college`
- `men_bachelors_degree`
- `women_bachelors_degree`
- `men_advanced_degree`
- `women_advanced_degree`

## 📦 Libraries Used

- `tidyverse`: For data manipulation and visualization
- `here`: For constructing portable file paths

To install the required packages, run:

```r
install.packages("tidyverse")
install.packages("here")

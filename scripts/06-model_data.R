#### Preamble ####
# Purpose: Models
# Author: Maria Mangru
# Date: 25 November 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
analysis_data <- read_csv("../data/02-analysis_data/analysis_data.csv")

# Filter data for Ontario region
ontario_data <- analysis_data %>% filter(region == "Ontario")

# Define binary variables
ontario_data <- ontario_data %>%
  mutate(
    In_Power = ifelse(recipient_in_power, 1, 0)
  )

# Aggregate total donations per party per year in Ontario
total_donations_ontario <- ontario_data %>%
  group_by(political_party, donation_year) %>%
  summarize(
    Total_Donations = sum(amount, na.rm = TRUE),
    In_Power = first(In_Power)
  ) %>%
  ungroup()

# Run Regression for Ontario
model_ontario <- lm(Total_Donations ~ In_Power, data = total_donations_ontario)
summary(model_ontario)


# Filter data for Federal region
federal_data <- analysis_data %>% filter(region == "Federal")

# Define binary variables
federal_data <- federal_data %>%
  mutate(
    In_Power = ifelse(recipient_in_power, 1, 0)
  )

# Aggregate total donations per party per year at Federal level
total_donations_federal <- federal_data %>%
  group_by(political_party, donation_year) %>%
  summarize(
    Total_Donations = sum(amount, na.rm = TRUE),
    In_Power = first(In_Power)
  ) %>%
  ungroup()

# Run Regression for Federal
model_federal <- lm(Total_Donations ~ In_Power, data = total_donations_federal)
summary(model_federal)





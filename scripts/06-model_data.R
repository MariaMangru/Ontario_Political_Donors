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
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")


# Filter data for Ontario
ontario_data <- donations_filtered %>% filter(region == "Ontario")

# Aggregate total donations per party per year in Ontario
total_donations_ontario <- ontario_data %>%
  group_by(political_party, donation_year) %>%
  summarize(
    Total_Donations = sum(amount, na.rm = TRUE),
    In_Power = first(recipient_in_power),
    Party_Size = first(Party_Size),
    Election_Year = first(Election_Year)
  ) %>%
  ungroup()

# Run Regression for Ontario with Controls
model_ontario <- lm(Total_Donations ~ In_Power + Party_Size + Election_Year, data = total_donations_ontario)
summary(model_ontario)


total_donations_ontario <- total_donations_ontario %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))  # Adding 1 to avoid log(0)

model_ontario <- lm(Log_Total_Donations ~ In_Power + Party_Size + Election_Year, data = total_donations_ontario)
summary(model_ontario)


# Filter data for Federal
federal_data <- donations_filtered %>% filter(region == "Federal")

# Aggregate total donations per party per year at Federal level
total_donations_federal <- federal_data %>%
  group_by(political_party, donation_year) %>%
  summarize(
    Total_Donations = sum(amount, na.rm = TRUE),
    In_Power = first(recipient_in_power),
    Party_Size = first(Party_Size),
    Election_Year = first(Election_Year)
  ) %>%
  ungroup()

# Run Regression for Federal with Controls
model_federal <- lm(Total_Donations ~ In_Power + Party_Size + Election_Year, data = total_donations_federal)
summary(model_federal)


# For Federal
total_donations_federal <- total_donations_federal %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))

model_federal <- lm(Log_Total_Donations ~ In_Power + Party_Size + Election_Year, data = total_donations_federal)
summary(model_federal)

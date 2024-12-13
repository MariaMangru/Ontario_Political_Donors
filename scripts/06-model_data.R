#### Preamble ####
# Purpose: Generates regression for donation amount and power status
# Author: Maria Mangru
# Date: 14 December 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(here)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")


#### Model for Ontario ####
# Filter data for Ontario
ontario_data <- analysis_data %>% 
  filter(region == "Ontario")

# Aggregate total donations per party per year in Ontario
total_donations_ontario <- ontario_data %>%
  group_by(political_party, donation_year) %>%
  summarize(
    Total_Donations = sum(real_amount, na.rm = TRUE),
    In_Power = first(recipient_in_power),
    Party_Size = first(Party_Size),
    Election_Year = first(Election_Year)
  ) %>%
  ungroup()

# Log-transform the dependent variable to stabilize variance
total_donations_ontario <- total_donations_ontario %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))  # Adding 1 to avoid log(0)

# Run Regression for Ontario with Controls
model_ontario <- lm(Log_Total_Donations ~ In_Power + Party_Size + Election_Year, data = total_donations_ontario)
summary(model_ontario2)


#### Model for Federal ####
# Filter data for Federal
federal_data <- analysis_data %>% 
  filter(region == "Federal")

# Aggregate total donations per party per year at Federal level
total_donations_federal <- federal_data %>%
  group_by(political_party, donation_year) %>%
  summarize(
    Total_Donations = sum(real_amount, na.rm = TRUE),
    In_Power = first(recipient_in_power),
    Party_Size = first(Party_Size),
    Election_Year = first(Election_Year)
  ) %>%
  ungroup()

# Log-transform the dependent variable to stabilize variance
total_donations_federal <- total_donations_federal %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))  # Adding 1 to avoid log(0)

# Run Regression for Federal with Controls
model_federal <- lm(Log_Total_Donations ~ In_Power + Party_Size + Election_Year, data = total_donations_federal)
summary(model_federal2)

#### Conservative and Liberal Analysis ####

# Filter for Conservative and Liberal parties
cl_data <- analysis_data %>%
  filter(political_party %in% c(
    "Conservative Party of Canada",
    "Liberal Party of Canada",
    "Progressive Conservative Party of Ontario",
    "Liberal Party of Ontario"
  ))

# Create variables
cl_data <- cl_data %>%
  mutate(
    In_Power = recipient_in_power,  # Already binary (1/0)
    Party = ifelse(
      political_party %in% c("Conservative Party of Canada", "Progressive Conservative Party of Ontario"),
      1,  # Conservative
      0   # Liberal
    )
  )

# Aggregate total donations per party per year
total_donations_cl <- cl_data %>%
  group_by(political_party, donation_year) %>%
  summarize(
    Total_Donations = sum(real_amount, na.rm = TRUE),
    In_Power = first(In_Power),
    Party = first(Party),
    Election_Year = first(Election_Year)
  ) %>%
  ungroup()

# Log-transform the dependent variable to stabilize variance
total_donations_cl <- total_donations_cl %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))  # Adding 1 to avoid log(0)

# Run the regression with interaction term
model_cl <- lm(Log_Total_Donations ~ In_Power * Party + Election_Year, data = total_donations_cl)
summary(model_cl2)


#### Save model ####

if (!dir.exists("models")) {
  dir.create("models")
}
# Save the Ontario model
saveRDS(model_ontario, file = "models/model_ontario.rds")

# Save the Federal model
saveRDS(model_federal, file = "models/model_federal.rds")

# Save the Conservative and Liberal model
saveRDS(model_cl, file = "models/model_cl.rds")

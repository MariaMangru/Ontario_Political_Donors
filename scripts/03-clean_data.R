#### Preamble ####
# Purpose: Cleans the raw political donations data
# Author: Maria Mangru
# Date: 25 November 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(dplyr)
library(lubridate)
library(readr)

#### Clean data ####
raw_data <- read_csv("data/01-raw_data/raw_data.csv")


# Filter for regions 'Federal' or 'Ontario'
donations_filtered <- raw_data %>%
  filter(region %in% c("Federal", "Ontario"))

# Ensure date columns are in Date format
donations_filtered$donation_date <- as.Date(donations_filtered$donation_date)

# Convert amount columns to numeric
donations_filtered$amount <- as.numeric(donations_filtered$amount)
donations_filtered$amount_monetary <- as.numeric(donations_filtered$amount_monetary)
donations_filtered$amount_non_monetary <- as.numeric(donations_filtered$amount_non_monetary)

# Replace NA values in amount columns with 0
donations_filtered <- donations_filtered %>%
  mutate(
    amount = ifelse(is.na(amount), 0, amount),
    amount_monetary = ifelse(is.na(amount_monetary), 0, amount_monetary),
    amount_non_monetary = ifelse(is.na(amount_non_monetary), 0, amount_non_monetary)
  )

# Convert donation_year to numeric
donations_filtered$donation_year <- as.numeric(donations_filtered$donation_year)

# Federal Government Parties in Power from 2011 to 2024
federal_government <- data.frame(
  year = 2011:2024,
  party_in_power = c(
    rep("Conservative Party of Canada", 4),  # 2011-2014
    rep("Liberal Party of Canada", 10)       # 2015-2024
  )
)

# Ontario Government Parties in Power from 2011 to 2024
ontario_government <- data.frame(
  year = 2011:2024,
  party_in_power = c(
    rep("Liberal Party of Ontario", 7),      # 2011-2017
    rep("Progressive Conservative Party of Ontario", 7)  # 2018-2024
  )
)

# Map party in power to each donation record
donations_filtered <- donations_filtered %>%
  mutate(
    party_in_power = case_when(
      region == "Federal" ~ federal_government$party_in_power[match(donation_year, federal_government$year)],
      region == "Ontario" ~ ontario_government$party_in_power[match(donation_year, ontario_government$year)],
      TRUE ~ NA_character_
    ),
    # Create a column indicating if the recipient party was in power
    recipient_in_power = political_party == party_in_power
  )





#### Save data ####
write_csv(cleaned_data, "outputs/data/analysis_data.csv")

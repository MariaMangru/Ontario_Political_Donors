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


# Convert donation_year to numeric
donations_filtered$donation_year <- as.numeric(donations_filtered$donation_year)


# Rename political_party entries for Ontario region
donations_filtered <- donations_filtered %>%
  mutate(
    political_party = case_when(
      region == "Ontario" & political_party %in% c("Ontario Liberal Party", "Liberal Party") ~ "Liberal Party of Ontario",
      region == "Ontario" & political_party == "New Democratic Party" ~ "New Democratic Party of Ontario",
      region == "Ontario" & political_party == "Bloc Québécois" ~ "Bloc Quebecois",
      region == "Federal" & political_party == "Bloc Québécois" ~ "Bloc Quebecois",  # Example for Federal region
      TRUE ~ political_party
    )
  )

# Add Party_Size Variable
donations_filtered <- donations_filtered %>%
  mutate(
    Party_Size = case_when(
      # Major Parties at the Federal Level
      region == "Federal" & political_party %in% c("Conservative Party of Canada", "Liberal Party of Canada", "New Democratic Party") ~ 1,
      # Major Parties at the Ontario Level
      region == "Ontario" & political_party %in% c("Progressive Conservative Party of Ontario", "Liberal Party of Ontario", "New Democratic Party of Ontario") ~ 1,
      # All Other Parties
      TRUE ~ 0
    )
  )

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
    # Create a binary column indicating if the recipient party was in power
    recipient_in_power = ifelse(political_party == party_in_power, 1, 0)
  )

donations_filtered <- donations_filtered %>%
  mutate(
    Election_Year = case_when(
      # Federal Election Years
      region == "Federal" & donation_year %in% c(2011, 2015, 2019, 2021) ~ 1,
      # Ontario Election Years
      region == "Ontario" & donation_year %in% c(2011, 2014, 2018, 2022) ~ 1,
      # All Other Years
      TRUE ~ 0
    )
  )

#### Save data ####
write_csv(donations_filtered, "data/02-analysis_data/analysis_data.csv")

#### Preamble ####
# Purpose: Cleans the raw political donations data
# Author: Maria Mangru
# Date: 12 December 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(dplyr)
library(lubridate)
library(readr)
library(here)
library(arrow)  

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
      region == "Federal" & political_party == "Bloc Québécois" ~ "Bloc Quebecois",
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

#### Define Government Parties in Power ####

# Federal Government Parties in Power from 2006 to 2024
federal_government <- data.frame(
  year = 2006:2024,
  party_in_power = c(
    # 2006-2007: Conservative
    rep("Conservative Party of Canada", 2),
    
    # 2008-2010: Conservative
    rep("Conservative Party of Canada", 3),
    
    # 2011-2014: Conservative
    rep("Conservative Party of Canada", 4),
    
    # 2015-2018: Liberal
    rep("Liberal Party of Canada", 4),
    
    # 2019-2020: Liberal
    rep("Liberal Party of Canada", 2),
    
    # 2021-2024: Liberal
    rep("Liberal Party of Canada", 4)
  )
)

# Ontario Government Parties in Power from 2003 to 2024
ontario_government <- data.frame(
  year = 2003:2024,
  party_in_power = c(
    # 2003-2006: Liberal
    rep("Liberal Party of Ontario", 4),
    
    # 2007-2010: Liberal
    rep("Liberal Party of Ontario", 4),
    
    # 2011-2013: Liberal
    rep("Liberal Party of Ontario", 3),
    
    # 2014-2017: Liberal
    rep("Liberal Party of Ontario", 4),
    
    # 2018-2021: Progressive Conservative
    rep("Progressive Conservative Party of Ontario", 4),
    
    # 2022-2024: Progressive Conservative
    rep("Progressive Conservative Party of Ontario", 3)
  )
)

# Map party in power to each donation record
donations_filtered <- donations_filtered %>%
  mutate(
    party_in_power = case_when(
      region == "Federal" & donation_year >= 2006 ~ federal_government$party_in_power[match(donation_year, federal_government$year)],
      region == "Ontario" & donation_year >= 2003 ~ ontario_government$party_in_power[match(donation_year, ontario_government$year)],
      TRUE ~ NA_character_
    ),
    # Create a binary column indicating if the recipient party was in power
    recipient_in_power = ifelse(political_party == party_in_power, 1, 0)
  )

# Define Election Years
donations_filtered <- donations_filtered %>%
  mutate(
    Election_Year = case_when(
      # Federal Election Years
      region == "Federal" & donation_year %in% c(2006, 2008, 2011, 2015, 2019, 2021) ~ 1,
      
      # Ontario Election Years
      region == "Ontario" & donation_year %in% c(2007, 2011, 2014, 2018, 2022) ~ 1,
      
      # All Other Years
      TRUE ~ 0
    )
  )


#  Convert Nominal Amounts to Real Amounts Manually

cpi_data <- data.frame(
  year = 2006:2024,
  CPI = c(109.1, 111.5, 114.1, 114.4, 116.5, 119.9, 121.7, 122.8, 125.2, 
          126.6, 128.4, 130.4, 133.4, 136.0, 137.0, 141.6, 151.2, 157.1, 161.80)
)

# Specify base year
base_year <- 2024

# Extract CPI for the base year
cpi_base <- cpi_data$CPI[cpi_data$year == base_year]

# Merge to add CPI to each donation record by its year
donations_filtered <- merge(donations_filtered, cpi_data, by.x = "donation_year", by.y = "year", all.x = TRUE)

# Convert nominal amounts to real amounts in 2024 dollars
donations_filtered$real_amount <- donations_filtered$amount * (cpi_base / donations_filtered$CPI)

#### Save data ####
write_csv(donations_filtered, "data/02-analysis_data/analysis_data.csv")

write_parquet(x = donations_filtered, sink = here("data", "02-analysis_data", "analysis_data.parquet"))


#### Preamble ####
# Purpose: Simulates a dataset of donations received by Federal and Ontario political parties
# Author: Maria Mangru
# Date: 25 November 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(dplyr)
library(lubridate)
library(stringi)
library(tibble)
set.seed(853)


#### Simulate data ####
# Define the years for the simulation
years <- 2010:2024

# Define political parties and their leadership change years
party_info <- tibble(
  political_party = c("Conservative Party of Canada", 
                      "Liberal Party of Canada", 
                      "New Democratic Party"),
  leader_change_years = list(
    c(2015, 2020),  # Leadership changes for Conservative Party
    c(2013, 2019),  # Leadership changes for Liberal Party
    c(2017)         # Leadership changes for NDP
  )
)

# Generate a data frame to map years to party leaders
leader_mapping <- data.frame(
  political_party = character(),
  donation_year = integer(),
  party_leader = character(),
  leader_change = logical(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(party_info)) {
  party <- party_info$political_party[i]
  change_years <- party_info$leader_change_years[[i]]
  
  # Assume the leader changes at the start of the change_year
  leader_years <- data.frame(
    political_party = party,
    donation_year = years,
    stringsAsFactors = FALSE
  )
  
  # Generate leader names (simulated)
  num_leaders <- length(change_years) + 1
  leaders <- paste("Leader", 1:num_leaders)
  
  # Assign leaders to years
  leader_years$party_leader <- NA
  current_leader_index <- 1
  for (year in years) {
    if (year %in% change_years) {
      current_leader_index <- current_leader_index + 1
    }
    leader_years$party_leader[leader_years$donation_year == year] <- leaders[current_leader_index]
  }
  
  # Mark years with leadership change
  leader_years$leader_change <- leader_years$donation_year %in% change_years
  
  # Combine into mapping
  leader_mapping <- bind_rows(leader_mapping, leader_years)
}

# Simulate number of donations per year
donations_per_year <- sample(500:1000, length(years), replace = TRUE)
total_donations <- sum(donations_per_year)

# Simulate the dataset
simulated_data <- data.frame(
  donation_id = 1:total_donations,                         
  region = rep("Federal", total_donations),                
  donation_year = sample(2000:2023, total_donations, replace = TRUE), 
  donation_date = as.Date("2000-01-01") + 
    sample(0:8730, total_donations, replace = TRUE),        
  donor_full_name = paste0("Donor_", 1:total_donations),    
  donor_type = rep("Individuals", total_donations),         
  amount = round(runif(total_donations, 10, 10000), 2),     
  political_party = sample(c("Conservative", "Liberal", "NDP"), 
                           total_donations, replace = TRUE), 
  party_leader = sample(c("Leader_1", "Leader_2", "Leader_3"), 
                        total_donations, replace = TRUE),   
  leader_change = sample(c(TRUE, FALSE), total_donations, replace = TRUE), 
  stringsAsFactors = FALSE
)

current_index <- 1
for (i in 1:length(years)) {
  year <- years[i]
  num_donations <- donations_per_year[i]
  donation_dates <- sample(seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by = "day"), num_donations, replace = TRUE)
  donor_names <- stri_rand_strings(num_donations, 10, pattern = "[A-Za-z]")
  
  # Simulate donation amounts
  amounts <- round(rexp(num_donations, rate = 1/200), 2)  # Average donation amount $200
  
  # Simulate political parties
  parties <- sample(party_info$political_party, num_donations, replace = TRUE)
  
  # Get party leaders and leadership change status
  leaders <- sapply(parties, function(party) {
    leader_mapping$party_leader[leader_mapping$political_party == party & leader_mapping$donation_year == year]
  })
  leader_changes <- sapply(parties, function(party) {
    leader_mapping$leader_change[leader_mapping$political_party == party & leader_mapping$donation_year == year]
  })
  
  # Fill in the data
  indices <- current_index:(current_index + num_donations - 1)
  simulated_data$donation_year[indices] <- year
  simulated_data$donation_date[indices] <- donation_dates
  simulated_data$donor_full_name[indices] <- donor_names
  simulated_data$amount[indices] <- amounts
  simulated_data$political_party[indices] <- parties
  simulated_data$party_leader[indices] <- leaders
  simulated_data$leader_change[indices] <- leader_changes
  
  current_index <- current_index + num_donations
}

# Clean up the data
simulated_data <- simulated_data %>%
  arrange(donation_date) %>%
  mutate(
    donation_id = row_number()
  )

#### Save data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")

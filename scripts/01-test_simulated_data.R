#### Preamble ####
# Purpose: Tests the structure and validity of the simulated political donation data electoral divisions dataset.
# Author: Maria Mangru
# Date: 12 December 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run


#### Workspace setup ####
library(dplyr)
library(lubridate)
library(testthat)
library(tidyverse)

simulated_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Convert columns to appropriate data types
simulated_data <- simulated_data %>%
  mutate(
    donation_date = as.Date(donation_date),
    donation_year = as.integer(donation_year),
    amount = as.numeric(amount),
    leader_change = as.logical(leader_change)
  )

# Begin testing
test_that("Data types are correct", {
  expect_type(simulated_data$donation_year, "integer")
  expect_s3_class(simulated_data$donation_date, "Date")
  expect_type(simulated_data$donor_full_name, "character")
  expect_type(simulated_data$donor_type, "character")
  expect_type(simulated_data$amount, "double")
  expect_type(simulated_data$political_party, "character")
  expect_type(simulated_data$party_leader, "character")
  expect_type(simulated_data$leader_change, "logical")
})

test_that("No missing values in critical columns", {
  critical_columns <- c("donation_id", "donation_year", "donation_date", "donor_full_name", "amount", "political_party", "party_leader", "leader_change")
  for (col in critical_columns) {
    expect_false(any(is.na(simulated_data[[col]])), info = paste("Missing values found in", col))
  }
})

test_that("Donation dates match donation years", {
  years_from_dates <- year(simulated_data$donation_date)
  expect_equal(simulated_data$donation_year, years_from_dates)
})

test_that("Donation amounts are positive", {
  expect_true(all(simulated_data$amount > 0))
})

test_that("Leader mapping is consistent", {
  # Check that for each political party and year, the party leader matches the expected leader
  unique_parties <- unique(simulated_data$political_party)
  for (party in unique_parties) {
    party_data <- simulated_data %>% filter(political_party == party)
    leader_years <- party_data %>% group_by(donation_year) %>% summarize(unique_leaders = n_distinct(party_leader))
    expect_true(all(leader_years$unique_leaders == 1), info = paste("Multiple leaders found for", party, "in a single year"))
  }
})

test_that("Leader change flags are correct", {
  # Verify that leader_change is TRUE only in years when leadership changed
  leader_changes <- simulated_data %>% filter(leader_change == TRUE)
  for (party in unique(leader_changes$political_party)) {
    change_years <- unique(leader_changes$donation_year[leader_changes$political_party == party])
    expected_change_years <- party_info$leader_change_years[[which(party_info$political_party == party)]]
    expect_equal(sort(change_years), sort(expected_change_years), info = paste("Leader change years mismatch for", party))
  }
})

test_that("Donor names are unique within a year", {
  donor_years <- simulated_data %>% group_by(donation_year, donor_full_name) %>% summarize(count = n())
  duplicates <- donor_years %>% filter(count > 1)
  expect_true(nrow(duplicates) == 0, info = "Duplicate donor names found within the same year")
})

test_that("Donor types are valid", {
  valid_types <- c("Individuals")
  expect_true(all(simulated_data$donor_type %in% valid_types))
})

test_that("Region is 'Federal' for all records", {
  expect_true(all(simulated_data$region == "Federal"))
})

test_that("Donation IDs are unique and sequential", {
  expect_equal(simulated_data$donation_id, 1:nrow(simulated_data))
})

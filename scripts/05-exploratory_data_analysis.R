#### Preamble ####
# Purpose: Explores the analysis dataset to determine any trends and patterns
# Author: Maria Mangru
# Date: 12 December 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(readr)   
library(tidyr)   

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

# Separate data into Ontario and Federal datasets
ontario_data <- analysis_data %>% filter(region == "Ontario")
federal_data <- analysis_data %>% filter(region == "Federal")


#### Exploring Ontario Data ####

### Summary Statistics ###
total_donations_ontario <- nrow(ontario_data)
total_amount_ontario <- sum(ontario_data$amount, na.rm = TRUE)
avg_donation_ontario <- mean(ontario_data$amount, na.rm = TRUE)

# Save Summary Statistics to a Text File
summary_stats_ontario <- data.frame(
  Metric = c("Total Donations", "Total Amount Donated", "Average Donation Amount"),
  Value = c(total_donations_ontario, total_amount_ontario, avg_donation_ontario)
)
write_csv(summary_stats_ontario, "data/02-analysis_data/02.1-exploration_data/Ontario/summary_statistics_ontario.csv")


#### Donations by Year ####
donations_by_year_ontario <- ontario_data %>%
  group_by(donation_year) %>%
  summarize(
    total_donations = n(),
    total_amount = sum(amount, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE)
  )

# Display Donations by Year
print(donations_by_year_ontario)
write_csv(donations_by_year_ontario, "data/02-analysis_data/02.1-exploration_data/Ontario/donations_by_year_ontario.csv")

## Donations by Political Party ##
donations_by_party_ontario <- ontario_data %>%
  group_by(political_party) %>%
  summarize(
    total_donations = n(),
    total_amount = sum(amount, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE)
  )

# Display Donations by Political Party
print(donations_by_party_ontario)
write_csv(donations_by_party_ontario, "data/02-analysis_data/02.1-exploration_data/Ontario/donations_by_political_party_ontario.csv")

## Donations Based on Recipient Power Status ##
donations_in_power_ontario <- ontario_data %>%
  group_by(recipient_in_power) %>%
  summarize(
    total_donations = n(),
    total_amount = sum(amount, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE)
  )

# Display Donations Based on Recipient Power Status
print(donations_in_power_ontario)
write_csv(donations_in_power_ontario, "data/02-analysis_data/02.1-exploration_data/Ontario/donations_in_power_status_ontario.csv")

### Visualizations ###

# Total Donations Over Years
p1_ontario <- ggplot(donations_by_year_ontario, aes(x = donation_year, y = total_amount)) +
  geom_line(group = 1, color = "steelblue") +  
  geom_point(color = "steelblue") +
  labs(title = "Total Donations Over Years - Ontario",
       x = "Year", y = "Total Amount Donated") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = donations_by_year_ontario$donation_year) +
  theme_minimal()

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Ontario/total_donations_over_years.png", plot = p1_ontario)

# Average Donation Amount Over Years
p2_ontario <- ggplot(donations_by_year_ontario, aes(x = donation_year, y = avg_amount)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Average Donation Amount Over Years - Ontario",
       x = "Year", y = "Average Donation Amount ($)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = donations_by_year_ontario$donation_year) +
  theme_minimal()

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Ontario/average_donation_amount_over_years.png", plot = p2_ontario)

# Donations by Political Party
p3_ontario <- ggplot(donations_by_party_ontario, aes(x = reorder(political_party, -total_amount), y = total_amount, fill = political_party)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Donations by Political Party - Ontario",
       x = "Political Party", y = "Total Amount Donated ($)") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Ontario/donations_by_political_party.png", plot = p3_ontario)

# Donations When Recipient is in Power vs. Not
p4_ontario <- ggplot(donations_in_power_ontario, aes(x = recipient_in_power, y = total_amount, fill = recipient_in_power)) +
  geom_bar(stat = "identity") +
  labs(title = "Donations Based on Party Power Status - Ontario",
       x = "Recipient Party in Power", y = "Total Amount Donated ($)") +
  scale_x_discrete(labels = c("FALSE" = "Not in Power", "TRUE" = "In Power")) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Ontario/donations_in_power_status.png", plot = p4_ontario)

# 5. Donation Amount Distribution
p5_ontario <- ggplot(ontario_data, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Donation Amounts - Ontario",
       x = "Donation Amount ($)", y = "Frequency") +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal()

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Ontario/donation_amount_distribution.png", plot = p5_ontario)

#### Exploring Federal Data ####

### Summary Statistics ###
total_donations_federal <- nrow(federal_data)
total_amount_federal <- sum(federal_data$amount, na.rm = TRUE)
avg_donation_federal <- mean(federal_data$amount, na.rm = TRUE)

# Save Summary Statistics to a Text File
summary_stats_federal <- data.frame(
  Metric = c("Total Donations", "Total Amount Donated", "Average Donation Amount"),
  Value = c(total_donations_federal, total_amount_federal, avg_donation_federal)
)
write_csv(summary_stats_federal, "data/02-analysis_data/02.1-exploration_data/Federal/summary_statistics_federal.csv")

## Donations by Year ##
donations_by_year_federal <- federal_data %>%
  group_by(donation_year) %>%
  summarize(
    total_donations = n(),
    total_amount = sum(amount, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE)
  )

# Display Donations by Year
print(donations_by_year_federal)
write_csv(donations_by_year_federal, "data/02-analysis_data/02.1-exploration_data/Federal/donations_by_year_federal.csv")

## Donations by Political Party ##
donations_by_party_federal <- federal_data %>%
  group_by(political_party) %>%
  summarize(
    total_donations = n(),
    total_amount = sum(amount, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE)
  )

# Display Donations by Political Party
print(donations_by_party_federal)
write_csv(donations_by_party_federal, "data/02-analysis_data/02.1-exploration_data/Federal/donations_by_political_party_federal.csv")

## Donations Based on Recipient Power Status ##
donations_in_power_federal <- federal_data %>%
  group_by(recipient_in_power) %>%
  summarize(
    total_donations = n(),
    total_amount = sum(amount, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE)
  )

# Display Donations Based on Recipient Power Status
print(donations_in_power_federal)
write_csv(donations_in_power_federal, "data/02-analysis_data/02.1-exploration_data/Federal/donations_in_power_status_federal.csv")

### Visualizations ###
# Total Donations Over Years
p1_federal <- ggplot(donations_by_year_federal, aes(x = donation_year, y = total_amount)) +
  geom_line(group = 1, color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Total Donations Over Years - Federal",
       x = "Year", y = "Total Amount Donated ($)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = donations_by_year_federal$donation_year) +
  theme_minimal()

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Federal/total_donations_over_years.png", plot = p1_federal)

# Average Donation Amount Over Years
p2_federal <- ggplot(donations_by_year_federal, aes(x = donation_year, y = avg_amount)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Average Donation Amount Over Years - Federal",
       x = "Year", y = "Average Donation Amount ($)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = donations_by_year_federal$donation_year) + 
  theme_minimal()

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Federal/average_donation_amount_over_years.png", plot = p2_federal)

# Donations by Political Party
p3_federal <- ggplot(donations_by_party_federal, aes(x = reorder(political_party, -total_amount), y = total_amount, fill = political_party)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Donations by Political Party - Federal",
       x = "Political Party", y = "Total Amount Donated ($)") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Federal/donations_by_political_party.png", plot = p3_federal)

# Donations When Recipient is in Power vs. Not
p4_federal <- ggplot(donations_in_power_federal, aes(x = recipient_in_power, y = total_amount, fill = recipient_in_power)) +
  geom_bar(stat = "identity") +
  labs(title = "Donations Based on Party Power Status - Federal",
       x = "Recipient Party in Power", y = "Total Amount Donated ($)") +
  scale_x_discrete(labels = c("FALSE" = "Not in Power", "TRUE" = "In Power")) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Federal/donations_in_power_status.png", plot = p4_federal)

# Donation Amount Distribution
p5_federal <- ggplot(federal_data, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Donation Amounts - Federal",
       x = "Donation Amount ($)", y = "Frequency") +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal()

ggsave(filename = "data/02-analysis_data/02.1-exploration_data/Federal/donation_amount_distribution.png", plot = p5_federal)




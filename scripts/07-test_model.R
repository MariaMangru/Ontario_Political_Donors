#### Preamble ####
# Purpose: Tests the validity of the models generated
# Author: Maria Mangru
# Date: 12 December 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded


#### Workspace setup ####
library(tidyverse)
library(caret)
library(car)
library(broom)
library(ggplot2)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

# Ensure 'amount' is numeric
analysis_data <- analysis_data %>%
  mutate(amount = as.numeric(amount))

#### Model for Ontario ####

# Filter data for Ontario
ontario_data <- analysis_data %>% 
  filter(region == "Ontario")

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

# Log-transform the dependent variable to stabilize variance
total_donations_ontario <- total_donations_ontario %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))

# Split data into training and test sets
set.seed(123)
train_index_ont <- createDataPartition(total_donations_ontario$Log_Total_Donations, p = 0.8, list = FALSE)
train_data_ont <- total_donations_ontario[train_index_ont, ]
test_data_ont <- total_donations_ontario[-train_index_ont, ]

# Run Regression for Ontario with Controls on training data
model_ontario <- lm(Log_Total_Donations ~ In_Power + Party_Size + Election_Year, data = train_data_ont)
summary(model_ontario)

# Predict on test data
test_data_ont$Predicted_Log_Total_Donations <- predict(model_ontario, newdata = test_data_ont)

# Calculate RMSE
rmse_ontario <- sqrt(mean((test_data_ont$Log_Total_Donations - test_data_ont$Predicted_Log_Total_Donations)^2))
print(paste("RMSE for Ontario model:", round(rmse_ontario, 4)))

# Model diagnostics for Ontario model
par(mfrow = c(2,2))
plot(model_ontario)
par(mfrow = c(1,1))

# Check for multicollinearity
vif_ontario <- vif(model_ontario)
print(vif_ontario)

# Alternative model with interaction term
model_ontario_alt <- lm(Log_Total_Donations ~ In_Power * Party_Size + Election_Year, data = train_data_ont)
summary(model_ontario_alt)

# Compare models using AIC
aic_ontario <- AIC(model_ontario, model_ontario_alt)
print(aic_ontario)

# Predict on test data with alternative model
test_data_ont$Predicted_Log_Total_Donations_Alt <- predict(model_ontario_alt, newdata = test_data_ont)

# Calculate RMSE for alternative model
rmse_ontario_alt <- sqrt(mean((test_data_ont$Log_Total_Donations - test_data_ont$Predicted_Log_Total_Donations_Alt)^2))
print(paste("RMSE for Ontario alternative model:", round(rmse_ontario_alt, 4)))

#### Model for Federal ####

# Filter data for Federal
federal_data <- analysis_data %>% 
  filter(region == "Federal")

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

# Log-transform the dependent variable to stabilize variance
total_donations_federal <- total_donations_federal %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))  # Adding 1 to avoid log(0)

# Split data into training and test sets
set.seed(123)
train_index_fed <- createDataPartition(total_donations_federal$Log_Total_Donations, p = 0.8, list = FALSE)
train_data_fed <- total_donations_federal[train_index_fed, ]
test_data_fed <- total_donations_federal[-train_index_fed, ]

# Run Regression for Federal with Controls on training data
model_federal <- lm(Log_Total_Donations ~ In_Power + Party_Size + Election_Year, data = train_data_fed)
summary(model_federal)

# Predict on test data
test_data_fed$Predicted_Log_Total_Donations <- predict(model_federal, newdata = test_data_fed)

# Calculate RMSE
rmse_federal <- sqrt(mean((test_data_fed$Log_Total_Donations - test_data_fed$Predicted_Log_Total_Donations)^2))
print(paste("RMSE for Federal model:", round(rmse_federal, 4)))

# Model diagnostics for Federal model
par(mfrow = c(2,2))
plot(model_federal)
par(mfrow = c(1,1))

# Check for multicollinearity
vif_federal <- vif(model_federal)
print(vif_federal)

# Alternative model with interaction term
model_federal_alt <- lm(Log_Total_Donations ~ In_Power * Party_Size + Election_Year, data = train_data_fed)
summary(model_federal_alt)

# Compare models using AIC
aic_federal <- AIC(model_federal, model_federal_alt)
print(aic_federal)

# Predict on test data with alternative model
test_data_fed$Predicted_Log_Total_Donations_Alt <- predict(model_federal_alt, newdata = test_data_fed)

# Calculate RMSE for alternative model
rmse_federal_alt <- sqrt(mean((test_data_fed$Log_Total_Donations - test_data_fed$Predicted_Log_Total_Donations_Alt)^2))
print(paste("RMSE for Federal alternative model:", round(rmse_federal_alt, 4)))

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
    Total_Donations = sum(amount, na.rm = TRUE),
    In_Power = first(In_Power),
    Party = first(Party),
    Election_Year = first(Election_Year)
  ) %>%
  ungroup()

# Log-transform the dependent variable to stabilize variance
total_donations_cl <- total_donations_cl %>%
  mutate(Log_Total_Donations = log(Total_Donations + 1))  # Adding 1 to avoid log(0)

# Split data into training and test sets
set.seed(123)
train_index_cl <- createDataPartition(total_donations_cl$Log_Total_Donations, p = 0.8, list = FALSE)
train_data_cl <- total_donations_cl[train_index_cl, ]
test_data_cl <- total_donations_cl[-train_index_cl, ]

# Run the regression with interaction term on training data
model_cl <- lm(Log_Total_Donations ~ In_Power * Party + Election_Year, data = train_data_cl)
summary(model_cl)

# Predict on test data
test_data_cl$Predicted_Log_Total_Donations <- predict(model_cl, newdata = test_data_cl)

# Calculate RMSE
rmse_cl <- sqrt(mean((test_data_cl$Log_Total_Donations - test_data_cl$Predicted_Log_Total_Donations)^2))
print(paste("RMSE for Conservative-Liberal model:", round(rmse_cl, 4)))

# Model diagnostics for Conservative-Liberal model
par(mfrow = c(2,2))
plot(model_cl)
par(mfrow = c(1,1))

# Check for multicollinearity
vif_cl <- vif(model_cl)
print(vif_cl)

# Alternative model without interaction term
model_cl_alt <- lm(Log_Total_Donations ~ In_Power + Party + Election_Year, data = train_data_cl)
summary(model_cl_alt)

# Compare models using AIC
aic_cl <- AIC(model_cl, model_cl_alt)
print(aic_cl)

# Predict on test data with alternative model
test_data_cl$Predicted_Log_Total_Donations_Alt <- predict(model_cl_alt, newdata = test_data_cl)

# Calculate RMSE for alternative model
rmse_cl_alt <- sqrt(mean((test_data_cl$Log_Total_Donations - test_data_cl$Predicted_Log_Total_Donations_Alt)^2))
print(paste("RMSE for Conservative-Liberal alternative model:", round(rmse_cl_alt, 4)))

#### Model Selection and Interpretation ####

# Compare RMSE and AIC to select the best model for each case

# Ontario Model Selection
if (rmse_ontario < rmse_ontario_alt) {
  best_model_ontario <- model_ontario
  print("Selected Ontario model without interaction term based on lower RMSE.")
} else {
  best_model_ontario <- model_ontario_alt
  print("Selected Ontario model with interaction term based on lower RMSE.")
}

# Federal Model Selection
if (rmse_federal < rmse_federal_alt) {
  best_model_federal <- model_federal
  print("Selected Federal model without interaction term based on lower RMSE.")
} else {
  best_model_federal <- model_federal_alt
  print("Selected Federal model with interaction term based on lower RMSE.")
}

# Conservative-Liberal Model Selection
if (rmse_cl < rmse_cl_alt) {
  best_model_cl <- model_cl
  print("Selected Conservative-Liberal model with interaction term based on lower RMSE.")
} else {
  best_model_cl <- model_cl_alt
  print("Selected Conservative-Liberal model without interaction term based on lower RMSE.")
}
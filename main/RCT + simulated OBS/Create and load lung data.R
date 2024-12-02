# Load required libraries
library(survival)
library(dplyr)

# Step 1: Load a real RCT dataset
data("lung") # Dataset from the survival package
real_rct <- lung %>%
  mutate(
    A = ifelse(inst > median(inst), 1, 0), # Simulated treatment (randomized)
    Y = ifelse(status == 2, 1, 0)         # Outcome (death)
  ) %>%
  select(A, Y, age, sex)

# Rename variables for consistency with IPSW notation
real_rct <- real_rct %>%
  rename(V = A, Y = Y, X1 = age, X2 = sex) %>%
  mutate(study = "RCT")

# Step 2: Simulate observational study data
set.seed(42)
n_obs <- 200
sim_obs <- tibble(
  X1 = runif(n_obs, min = min(real_rct$X1), max = max(real_rct$X1)), # Age
  X2 = sample(real_rct$X2, n_obs, replace = TRUE),                   # Sex
  A = rbinom(n_obs, 1, plogis(-0.05 * X1 + 0.3 * X2)),               # Treatment assignment based on age & sex
  Y = rbinom(n_obs, 1, plogis(0.5 * A + 0.02 * X1 - 0.1 * X2))       # Outcome depends on A, age, and sex
) %>%
  rename(V = A) %>%
  mutate(study = "OBS")

# Step 3: Combine the datasets
combined_data <- bind_rows(real_rct, sim_obs)

# Step 4: Save combined dataset
saveRDS(combined_data, "combined_data.rds")

# Step 5: Quick Summary
summary(combined_data)

# Step 6: Inspect combined data
head(combined_data)

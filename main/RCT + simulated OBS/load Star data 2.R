# Step 1: Load necessary libraries
library(dplyr)
library(haven)

star_students <- read_sav("lib/STAR_Students.sav")

# Step 2: Preprocess the RCT dataset
# Select relevant columns
rct_data <- star_students %>%
  select(stdntid, gender, race, g1freelunch, g1classsize, g1treadss) %>%
  filter(!is.na(g1freelunch) & !is.na(gender) & !is.na(race)) %>%
  mutate(
    gender = as.numeric(gender),
    race = as.numeric(race),
    free_lunch = as.numeric(g1freelunch),
    class_size = as.numeric(g1classsize),
    reading_score = as.numeric(g1treadss)
  ) %>%
  select(stdntid, gender, race, free_lunch, class_size, reading_score)

# Step 3: Simulate observational study treatment assignment
# Use a logistic model based on covariates for non-random treatment assignment
set.seed(42)
obs_data <- rct_data %>%
  mutate(
    treatment = rbinom(n(), 1, plogis(-0.5 + 0.2 * gender + 0.3 * race - 0.4 * free_lunch + 0.1 * class_size)),
    outcome = rnorm(n(), mean = 10 + 2 * treatment + 0.1 * reading_score, sd = 5)
  )

# Step 4: Add a study indicator and combine with RCT data
rct_data <- rct_data %>%
  mutate(treatment = sample(0:1, n(), replace = TRUE), # Random treatment assignment
         outcome = rnorm(n(), mean = 10 + 2 * treatment, sd = 5),
         study = "RCT")

obs_data <- obs_data %>%
  mutate(study = "OBS")

combined_data <- bind_rows(rct_data, obs_data)

# Step 5: Inspect the combined data
summary(combined_data)
head(combined_data)

# Step 6: Save combined dataset
saveRDS(combined_data, "combined_star_students.rds")

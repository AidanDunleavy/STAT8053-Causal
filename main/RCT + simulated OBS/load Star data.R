library(tidyverse)
library(haven)
# Load STAR High Schools dataset
# star_high <- read_sav("lib/STAR_High_Schools.sav")
# 
# Load STAR K-3 Schools dataset
# star_k3 <- read_sav("lib/STAR_K-3_Schools.sav")

# Load STAR Students dataset
star_students <- read_sav("lib/STAR_Students.sav")

dim(star_students)

names(star_students)



indices <- grep(pattern = "^(g1tmath)|^(g1tlist)|^(g1tread)", x = names(star_students), value = FALSE)
label_by_column[indices]

attr(star_students[[1]], "label")

# star_students$g1tmathss

table(star_students[indices[3]], useNA = "always")
table(star_students$cmpstype, useNA = "always")
label_by_column$g1classtype

table(star_students$g1classsize, useNA = "always")
table(star_students$g1classtype, useNA = "always")

# Subset and rename variables for clarity
data_rct <- star_students %>%
  transmute(
    student_id = stdntid,
    treatment = case_when(
      g1classtype == 1 ~ 1,  # Small class
      TRUE ~ 0            # Regular class or regular with aide
    ),
    outcome = g1treadss + g1tmathss + g1tlistss,    # Grade 1 sum of reading score, math score, and listening score
    gender = as_factor(gender),
    race = as_factor(race),
    birth_year = birthyear,
    birth_month = birthmonth,
    free_lunch = as_factor(g1freelunch)
  ) %>%
  filter(!is.na(treatment), !is.na(outcome), !is.na(gender),
         !is.na(race), !is.na(birth_year), !is.na(birth_month), !is.na(free_lunch))

table(data_rct$treatment, useNA = "always")
table(data_rct$gender, useNA = "always")
table(data_rct$race, useNA = "always")
table(data_rct$birth_year, useNA = "always")
table(data_rct$birth_month, useNA = "always")
table(data_rct$free_lunch, useNA = "always")

dim(data_rct)

# Simulate observational treatment assignment
set.seed(123)

data_obs <- data_rct %>%
  mutate(
    # Remove original treatment assignment
    treatment_rct = treatment,
    # Create a propensity score based on covariates
    propensity_score = plogis(
      -0.5 + 0.4 * as.numeric(gender) +
        0.3 * as.numeric(race) +
        0.01 * birth_year +
        0.5 * as.numeric(data_rct$birth_month) +
        0.5 * as.numeric(free_lunch)
    ),
    # Assign treatment based on propensity score
    treatment = rbinom(n(), 1, propensity_score)
  )



lapply(as.numeric(data_rct$free_lunch), function(x) {
  
})

table(star_students$g4nfreelunch, useNA = "always")

# Add study indicator
data_rct <- data_rct %>% mutate(study = "RCT")
data_obs <- data_obs %>% mutate(study = "OBS")

# Combine datasets
combined_data <- bind_rows(data_rct, data_obs)

# Summary statistics
combined_data %>%
  group_by(study, treatment) %>%
  summarise(
    mean_outcome = mean(outcome, na.rm = TRUE),
    count = n()
  )

# Visualize distributions
ggplot(combined_data, aes(x = outcome, fill = study)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ treatment) +
  theme_minimal()

######################################

# Estimate propensity scores in observational data
ps_model <- glm(
  treatment ~ gender + race + birth_year + free_lunch,
  data = data_obs,
  family = binomial()
)

data_obs <- data_obs %>%
  mutate(
    propensity_score = predict(ps_model, type = "response"),
    weight = ifelse(treatment == 1,
                    1 / propensity_score,
                    1 / (1 - propensity_score))
  )

# Weighted outcome mean
weighted_mean <- data_obs %>%
  group_by(treatment) %>%
  summarise(
    weighted_outcome = sum(outcome * weight) / sum(weight)
  )



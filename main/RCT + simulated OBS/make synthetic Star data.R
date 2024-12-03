# Step 1: Load necessary libraries
library(dplyr)
library(haven)
star_students <- read_sav("lib/STAR_Students.sav")

label_by_column <-setNames(lapply(names(star_students), function(index) {
  c(attr(star_students[[index]], "labels"),
    attr(star_students[[index]], "label"))
}), names(star_students))

label_by_column$g1classtype


# Step 2: Create synthetic RCT
# Assuming `star_students` is the loaded dataset


# Define the covariate to split samples
star_students <- star_students %>%
  mutate(
    U = ifelse(g1surban %in% c(1, 3), 1, 0) # 1: rural/inner-city, 0: urban/suburban
  )

table(star_students$U, useNA = "always")
table(star_students$g1treadss, useNA = "always")

# Subset and rename variables for clarity
star_students_filtered <- star_students %>%
  transmute(
    student_id = stdntid,
    treatment = case_when(
      g1classtype == 1 ~ 1,  # Small class
      TRUE ~ 0            # Regular class or regular with aide
    ),
    outcome = g1treadss + g1tmathss + g1tlistss,    # Grade 1 sum of reading score, math score, and listening score
    gender = as_factor(gender),
    race = as_factor(race),
    birth_month = birthmonth,
    birth_day = birthday,
    birth_year = birthyear,
    free_lunch = as_factor(g1freelunch),
    teacher_id = as_factor(g1tchid),
    U = U
  ) %>%
  filter(!is.na(treatment), !is.na(outcome), !is.na(gender),
         !is.na(race), !is.na(birth_year), !is.na(birth_month),
         !is.na(free_lunch), !is.na(teacher_id))

table(star_students_filtered$U, useNA = "always")
dim(star_students_filtered)

seed = 12345
set.seed(seed) # For reproducibility
# Randomly sample half of the U=1 group for the RCT
rct_data <- star_students_filtered %>%
  filter(U == 1) %>%
  sample_frac(0.5) %>%
  mutate(study = "RCT")

dim(rct_data)




# Step 3: Create synthetic observational study (OS)
# Part (a): U == 1
# then if A = 0, keep as long as it is not in RCT.
# But if A = 1, keep if outcome is in (lower half of samples with A = 1 and U = 1). 
os_data_part_a <- star_students_filtered %>%
  filter(U == 1) %>%
  group_by(A = treatment == 1) %>%
  mutate(
    lower_half_cutoff = ifelse(A == 1, quantile(outcome[A == 1], probs = 0.5, na.rm = TRUE), NA)
  ) %>%
  ungroup() %>%
  filter(
    # Include A = 0 if not in RCT
    (A == 0 & !student_id %in% rct_data$student_id) |
      # Include A = 1 if in lower half of outcomes for A = 1 and U = 1
      (A == 1 & outcome <= lower_half_cutoff)
  )

dim(os_data_part_a)


intersect(os_data_part_a$student_id, rct_data$student_id)

# Part (b): From samples with U = 0, take all samples with A = 0 and samples with A = 1 whose outcomes
# are in the lower half of outcomes among samples with A = 1 & U = 0.
os_data_part_b <- star_students_filtered %>%
  filter(U == 0) %>%
  group_by(A = treatment == 1) %>%
  mutate(
    lower_half_cutoff = ifelse(A == 1, quantile(outcome[A == 1], probs = 0.5, na.rm = TRUE), NA)
  ) %>%
  ungroup() %>%
  filter(
    # Include A = 0 if not in RCT
    (A == 0) |
      # Include A = 1 if in lower half of outcomes for A = 1 and U = 1
      (A == 1 & outcome <= lower_half_cutoff)
  )


# Combine OS data
os_data <- bind_rows(os_data_part_a, os_data_part_b) %>%
  mutate(study = "OS")

intersect(os_data_part_b$student_id, rct_data$student_id)


# Step 4: Combine RCT and OS datasets
combined_data <- bind_rows(rct_data, os_data)[-c(13,14)] # removes "A" and "lower_half_cutoff"

# Step 5: Check and save the combined dataset
summary(combined_data)
saveRDS(combined_data, paste0("lib/synthetic_star_data",seed,".rds"))

library(dplyr)
library(haven)
library(forcats)

star_students <- read_sav("lib/STAR_Students.sav")

MakeSyntheticSTAR <- function(star_students) {
  
  # Define the covariate to split samples
  star_students <- star_students %>%
    mutate(
      U = ifelse(g1surban %in% c(1, 3), 1, 0) # 1: rural/inner-city, 0: urban/suburban
    )
  
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
      # teacher_id = as_factor(g1tchid),
      U = U
    ) %>%
    filter(!is.na(treatment), !is.na(outcome), !is.na(gender),
           !is.na(race), !is.na(birth_year), !is.na(birth_month),
           !is.na(free_lunch))#, !is.na(teacher_id))
  
  # Randomly sample half of the U=1 group for the RCT
  rct_data <- star_students_filtered %>%
    filter(U == 1) %>%
    sample_frac(0.5) %>%
    mutate(study = "RCT")
  
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
        # Include A = 1 if in lower half of outcomes for A = 1 and U = 0
        (A == 1 & outcome <= lower_half_cutoff)
    )
  
  # Combine OS data
  os_data <- bind_rows(os_data_part_a, os_data_part_b) %>%
    mutate(study = "OS")
  
  # Step 4: Combine RCT and OS datasets
  combined_data <- bind_rows(rct_data, os_data) %>% select(-c(A, lower_half_cutoff)) # removes "A" and "lower_half_cutoff"
  
  combined_data <- data %>%
    transmute(
      # id = student_id,
      V = as.numeric(as.factor(study)) - 1L, # V = 1 implies in RCT: V = 0 implies in OS.
      X1 = gender,
      X2 = fct_collapse(
        race,
        OTHER = c("ASIAN", "HISPANIC", "NATIVE AMERICAN", "OTHER"),
        WHITE = "WHITE",
        BLACK = "BLACK"
      ),
      X3 = birth_month,
      X4 = birth_day,
      X5 = birth_year,
      X6 = free_lunch,
      # X7 = teacher_id,
      A = treatment,
      Y = outcome
    )
  
  return(combined_data)
}


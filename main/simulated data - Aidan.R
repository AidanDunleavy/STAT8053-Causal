# Load necessary libraries
set.seed(123)  # For reproducibility

# Parameters
n_rct <- 200      # Sample size for the RCT
n_obs <- 500      # Sample size for the observational study
treatment_effect <- 5  # Treatment effect on the outcome

# Baseline covariates
age_mean <- 50
age_sd <- 10
sex_prob <- 0.5    # Probability of being male
income_mean <- 50000
income_sd <- 10000

# Generate RCT dataset
rct_data <- data.frame(
  id = 1:n_rct,
  study = "RCT",
  age = rnorm(n_rct, mean = age_mean, sd = age_sd),
  sex = rbinom(n_rct, 1, sex_prob),
  income = rnorm(n_rct, mean = income_mean, sd = income_sd),
  treatment = rbinom(n_rct, 1, 0.5)  # Randomly assigned treatment (0 = control, 1 = treated)
)

# Outcome for RCT dataset
rct_data$outcome <- 50 + 0.3 * rct_data$age + 3 * rct_data$sex + 0.0001 * rct_data$income +
  treatment_effect * rct_data$treatment + rnorm(n_rct, sd = 5)

# Generate observational dataset
obs_data <- data.frame(
  id = (n_rct + 1):(n_rct + n_obs),
  study = "Observational",
  age = rnorm(n_obs, mean = age_mean, sd = age_sd),
  sex = rbinom(n_obs, 1, sex_prob),
  income = rnorm(n_obs, mean = income_mean, sd = income_sd)
)

# Treatment assignment in the observational study based on age and income (non-random)
obs_data$treatment <- ifelse(obs_data$age > 55 | obs_data$income > 60000, 1, 0)

# Outcome for observational dataset
obs_data$outcome <- 50 + 0.3 * obs_data$age + 3 * obs_data$sex + 0.0001 * obs_data$income +
  treatment_effect * obs_data$treatment + rnorm(n_obs, sd = 5)

# Combine RCT and observational datasets
combined_data <- rbind(rct_data, obs_data)

# Convert categorical variables
combined_data$sex <- factor(combined_data$sex, levels = c(0, 1), labels = c("Female", "Male"))
combined_data$treatment <- factor(combined_data$treatment, levels = c(0, 1), labels = c("Control", "Treated"))

# Display summary of the combined dataset
summary(combined_data)
head(combined_data)


str(combined_data)

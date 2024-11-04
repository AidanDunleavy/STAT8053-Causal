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
  V = 1,  # Study group indicator (1 = RCT, 0 = Observational)
  X1 = rnorm(n_rct, mean = age_mean, sd = age_sd),
  X2 = rbinom(n_rct, 1, sex_prob),
  X3 = rnorm(n_rct, mean = income_mean, sd = income_sd),
  A = rbinom(n_rct, 1, 0.5)  # Randomly assigned treatment (0 = control, 1 = treated)
)

# Outcome for RCT dataset
rct_data$Y <- 50 + 0.3 * rct_data$X1 + 3 * rct_data$X2 + 0.0001 * rct_data$X3 +
  treatment_effect * rct_data$A + rnorm(n_rct, sd = 5)

# Generate observational dataset
obs_data <- data.frame(
  id = (n_rct + 1):(n_rct + n_obs),
  V = 0,  # Study group indicator (1 = RCT, 0 = Observational)
  X1 = rnorm(n_obs, mean = age_mean, sd = age_sd),
  X2 = rbinom(n_obs, 1, sex_prob),
  X3 = rnorm(n_obs, mean = income_mean, sd = income_sd)
)

# Treatment assignment in the observational study based on X1 (age) and X3 (income)
obs_data$A <- ifelse(obs_data$X1 > 55 | obs_data$X3 > 60000, 1, 0)

# Outcome for observational dataset
obs_data$Y <- 50 + 0.3 * obs_data$X1 + 3 * obs_data$X2 + 0.0001 * obs_data$X3 +
  treatment_effect * obs_data$A + rnorm(n_obs, sd = 5)

# Combine RCT and observational datasets
combined_data <- rbind(rct_data, obs_data)

# Convert categorical variables
# combined_data$X2 <- factor(combined_data$X2, levels = c(0, 1), labels = c("Female", "Male"))
# combined_data$A <- factor(combined_data$A, levels = c(0, 1), labels = c("Control", "Treated"))

# Display summary of the combined dataset
summary(combined_data)
head(combined_data)

# Save the combined dataset as an RDS file
# saveRDS(combined_data, file = "lib/combined_data.rds")


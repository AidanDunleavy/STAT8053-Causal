simulateData <- function(n_rct, n_obs, treatment_effect, age_mean, age_sd, sex_prob, income_mean, income_sd) {
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
  combined_data <- combined_data[, !names(combined_data) %in% c("id")]
  return(combined_data)
}


repsim <- function(rep) {
  mat <- c(0,0,0)
  for(i in 1:rep) {
    DF <- simulateData(200, 500, 5, 50, 10, 0.5, 50000, 10000)
    if (i == 1) {
      mat <- c(compute_ipsw(DF, normalized = TRUE, estimation = "logit", covariates = "All"),
               compute_gformula(DF),
               compute_aipsw(DF, p=0))
    } else {
      mat <- rbind(mat,c(compute_ipsw(DF, normalized = TRUE, estimation = "logit", covariates = "All"),
                         compute_gformula(DF),
                         compute_aipsw(DF, p=0)))
    }
  }
  return(mat)
}

estimates <- repsim(1000)
estimates <- data.frame(estimates)
names(estimates) <- c("IPSW", "Gformula", "AIPSW")
boxplot(estimates)
abline(h=5, lty = 2, col = "red")

library(reshape2)
library(MASS)
library(dplyr)
library(ggplot2)  # Load ggplot2 for cut_number function
library(table1) # table for baseline
library(wesanderson) # colors

results_long <- melt(estimates, variable.name = "Method", value.name = "ATE")

# Choose a color palette from wesanderson
palette_colors <- wes_palette("Darjeeling1", 3, type = "discrete")

# Create the box plot with wesanderson colors and custom legend
ggplot(results_long, aes(x = ATE, y = Method, fill = Method)) +
  geom_boxplot() +
  # Add a dummy geom for Population ATE line to appear in the legend
  geom_vline(aes(xintercept = 5, color = "Population ATE"), linetype = "dashed", size = 0.5) + # Adjust 27.0 to your actual population ATE value
  labs(x = "Estimated ATE", y = "", fill = "Method", color = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(fill = NA, size = 1)
  ) +
  # Set fill colors for boxplots
  scale_fill_manual(values = palette_colors) +
  # Add custom color for the dashed line in the legend
  scale_color_manual(values = c("Population ATE" = "red")) +
  # Customize legend to include boxplot fills and the dashed line
  guides(
    fill = guide_legend(
      override.aes = list(shape = 21, size = 5, color = "black", fill = palette_colors)
    ),
    color = guide_legend(
      override.aes = list(linetype = "dashed", size = 0.5)
    )
  )

estimates

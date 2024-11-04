# Load the combined dataset
combined_data <- readRDS("lib/combined_data_large.rds")

# Fit a logistic regression model to estimate the probability of being in the RCT sample
# X1 = age, X2  = sex, X3 = incomde, V == 1 -> in RCT, V == 0 -> obs. Y is treatment effect. 
selection_model <- glm(V == 1 ~ X1 + X2 + X3, 
                       data = combined_data, 
                       family = binomial)

# Predict the probabilities of being in the RCT for each observation
combined_data$ipsw_weight <- ifelse(
  combined_data$V == 0,
  1 / (1 - predict(selection_model, type = "response")),
  1  # Weight is 1 for RCT observations
)

# Weighted linear regression to estimate the treatment effect
ipsw_model <- lm(Y ~ V, data = combined_data, weights = ipsw_weight)

# Summary of the weighted model
summary(ipsw_model)

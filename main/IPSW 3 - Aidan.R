compute_ipsw <- function(DF, normalized = FALSE, estimation = "logit", covariates = "All") {
  
  N <- nrow(DF)
  n <- nrow(DF[DF$study == "RCT", ])  # Count for RCT sample
  m <- nrow(DF[DF$study == "Observational", ])  # Count for observational sample
  
  # Select covariates based on input
  if (covariates == "All") {
    temp <- DF
  } else if (covariates == "age") {
    temp <- DF[, c("age", "study", "treatment", "outcome")]
  } else if (covariates == "-age") {
    temp <- DF[, !(names(DF) %in% c("age"))]
  } else {
    print("Covariates parameter must be All, age, or -age.")
    return(NULL)
  }
  
  # Estimation of P(study = "RCT" | covariates)
  if (estimation == "logit") {
    # Logistic regression to estimate the probability of being in the RCT sample
    p.fit <- glm(study == "RCT" ~ ., family = binomial("logit"), 
                 data = temp[, !names(temp) %in% c("treatment", "outcome")])
    p <- predict(p.fit, type = "response", newdata = temp)
  } else if (estimation == "forest") {
    print("Random forest estimation is not implemented in this version.")
    return(NULL)
  } else {
    print("Estimation parameter should be 'logit' or 'forest'.")
    return(NULL)
  }
  
  # Calculate IPSW weights
  DF$ipsw_weight <- ifelse(DF$study == "Observational", 1 / (1 - p), 1)
  
  # Normalize weights if required
  if (normalized) {
    DF$ipsw_weight <- DF$ipsw_weight / mean(DF$ipsw_weight[DF$study == "Observational"])
  }
  
  return(DF$ipsw_weight)
}


# Load combined_data if not already loaded
combined_data <- readRDS("lib/combined_data.rds")

# Compute IPSW weights
combined_data$ipsw_weight <- compute_ipsw(combined_data, normalized = TRUE, estimation = "logit", covariates = "All")

# Check the first few weights
head(combined_data$ipsw_weight)








library(glmnet)

compute_ipsw <- function(DF, normalized = FALSE, estimation = "logit", covariates = "All") {
  
  N <- nrow(DF)
  n <- nrow(DF[DF$study == "RCT", ])  # Count for RCT sample
  m <- nrow(DF[DF$study == "Observational", ])  # Count for observational sample
  
  # Select covariates based on input
  if (covariates == "All") {
    temp <- DF
  } else if (covariates == "age") {
    temp <- DF[, c("age", "study", "treatment", "outcome")]
  } else if (covariates == "-age") {
    temp <- DF[, !(names(DF) %in% c("age"))]
  } else {
    print("Covariates parameter must be All, age, or -age.")
    return(NULL)
  }
  
  # Standardize covariates
  covariate_cols <- c("age", "sex", "income")  # Specify covariate columns here
  temp[covariate_cols] <- scale(temp[covariate_cols])
  
  # Estimation of P(study = "RCT" | covariates)
  if (estimation == "logit") {
    # Logistic regression with glmnet for regularization
    X <- model.matrix(study == "RCT" ~ ., data = temp[, !names(temp) %in% c("treatment", "outcome")])[, -1]
    y <- as.numeric(temp$study == "RCT")
    p.fit <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)  # alpha = 0.5 for elastic net regularization
    p <- predict(p.fit, newx = X, type = "response", s = "lambda.min")
  } else if (estimation == "forest") {
    print("Random forest estimation is not implemented in this version.")
    return(NULL)
  } else {
    print("Estimation parameter should be 'logit' or 'forest'.")
    return(NULL)
  }
  
  # Calculate IPSW weights
  DF$ipsw_weight <- ifelse(DF$study == "Observational", 1 / (1 - p), 1)
  
  # Normalize weights if required
  if (normalized) {
    DF$ipsw_weight <- DF$ipsw_weight / mean(DF$ipsw_weight[DF$study == "Observational"])
  }
  
  return(DF$ipsw_weight)
}

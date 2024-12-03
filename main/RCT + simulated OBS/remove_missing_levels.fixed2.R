remove_missing_levels.fixed2 <- function(fit, test_data) {
  
  # Ensure test_data is a data.frame and drop empty factor levels
  test_data <- test_data %>%
    droplevels() %>%
    as.data.frame()
  
  # Handle different model types (e.g., lm, glmmPQL)
  if (any(class(fit) == "glmmPQL")) {
    # Extract factor predictors and levels
    factors <- gsub("as.factor\\(|\\)", "", names(fit$contrasts)) # Keep numeric endings intact
    
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unlist(lapply(fit$contrasts, function(x) names(x)))
    model_factors <- data.frame(factors = rep(factors, each = length(factor_levels) / length(factors)),
                                factor_levels = factor_levels,
                                stringsAsFactors = FALSE)
  } else {
    # Extract factor predictors and levels for other model types (e.g., lm)
    factors <- gsub("as.factor\\(|\\)", "", names(fit$xlevels)) # Keep numeric endings intact
    
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- data.frame(factors = rep(factors, each = length(factor_levels) / length(factors)),
                                factor_levels = factor_levels,
                                stringsAsFactors = FALSE)
  }
  
  # Select factor columns in test_data that match predictors in the model
  predictors <- intersect(names(test_data), factors)
  
  # For each factor predictor, check and adjust levels
  for (predictor in predictors) {
    # Find which levels are not present in the model
    valid_levels <- model_factors[model_factors$factors == predictor, ]$factor_levels
    invalid_rows <- !test_data[[predictor]] %in% valid_levels
    
    if (any(invalid_rows)) {
      # Set invalid levels to NA
      test_data[invalid_rows, predictor] <- NA
      # Drop empty factor levels
      test_data[[predictor]] <- droplevels(test_data[[predictor]])
      
      # Issue a warning
      message(sprintf("Setting missing levels in '%s' (test data, but not in train data) to NA.", predictor))
    }
  }
  
  return(test_data)
}
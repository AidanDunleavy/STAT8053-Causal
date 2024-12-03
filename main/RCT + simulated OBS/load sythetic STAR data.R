library(reshape2)
library(MASS)
library(dplyr)
library(table1) # table for baseline
library(wesanderson) # colors
library(genRCT) # remotes::install_github("idasomm/genRCT")
library(ggplot2)  # Load ggplot2 for cut_number function
library(wesanderson) # colors
library(haven)
library(forcats)
library(future.apply)
# library(sperrorest) # for the remove_missing_levels function but sadly doesnt work if column name ends in number (WHY????)
# library(lme4)

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
    num_levels <- lengths(fit$contrasts)  # Number of levels per factor
    model_factors <- data.frame(
      factors = rep(factors, num_levels),
      factor_levels = factor_levels,
      stringsAsFactors = FALSE
    )
  } else {
    # Extract factor predictors and levels for other model types (e.g., lm)
    factors <- gsub("as.factor\\(|\\)", "", names(fit$xlevels)) # Keep numeric endings intact
    
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    num_levels <- sapply(fit$xlevels, length)  # Number of levels per factor
    model_factors <- data.frame(
      factors = rep(factors, num_levels),
      factor_levels = factor_levels,
      stringsAsFactors = FALSE
    )
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
      # message(sprintf("Setting missing levels in '%s' (test data, but not in train data) to NA.", predictor))
    }
  }
  
  return(test_data)
}


compute_mean_diff_RCT <- function(DF){
  RCT_ATE <- mean(DF[DF$A == 1 & DF$V == 1, "Y"]) - mean(DF[DF$A == 0  & DF$V == 1, "Y"])  
  return(RCT_ATE)
}

# IPSW: covariates parameters is for simulation, to restrict only to X1 or not the estimation. 
compute_ipsw <- function(DF, normalized = FALSE, estimation = "logit", covariates = "All"){
  
  N <- nrow(DF)
  n <- nrow(DF[DF$V ==1, ])
  m <- nrow(DF[DF$V ==0, ])
  
  ## simply delete this paragraph when removal of the covariates parameter
  if (covariates == "All"){
    temp <- DF
  } else if (covariates == "X1"){
    temp <- DF[, c("X1", "V", "A", "Y")]
  } else if (covariates == "-X1"){
    temp <- DF[, !(names(DF) %in%  c("X1"))]
  } else {
    print("Covariates parameters must be All, X1, -X1.")
    break
  }
  
  # Estimation of P(V = 1 | X)
  # p <-- P(V = 1 | X) 
  
  if (estimation == "logit"){
    
    # with logistic regression
    p.fit  <- glm(V ~., family = binomial("logit"), data = temp[, !names(temp) %in% c("A", "Y")])
    p <- predict(p.fit, type = "response", newdata = temp)
    
  } 
  else if (estimation == "forest") {
    break
  } 
  else {
    print("Estimation parameter should be forest or logit.")
    break
  }
  
  # Store odds
  temp$odds <- ((1 - p)/p)
  
  # Keep only RCT for the rest of the calculus
  temp <- temp[temp$V == 1,]
  
  if (normalized == FALSE){
    tau_ipsw <- (2/m)*with(temp, sum(odds*A*Y - odds*(1-A)*Y))  
  } else {
    tau_ipsw <- with(temp, sum(odds*A*Y/sum(odds*A) - odds*(1-A)*Y/sum(odds*(1-A))))
  }
  
  return(tau_ipsw)
}


# STRATIFICATION
compute_stratification <- function(DF, nb_strat = 10, bin = "quantile"){
  
  temp <- DF
  
  # logit : sampling score
  # temp$V <- as.numeric(temp$V)
  
  pi_s_reg <- glm(V ~ ., family = "binomial", data = temp[, !names(temp) %in% c("Y", "A")])
  
  # following lines are equivalent to predict
  #pi_s_coefficient <- pi_s_reg$coefficients
  #X <- as.matrix(temp[, !names(temp) %in% c("Y", "A", "S")])
  #pi_strat <- as.numeric(expit(cbind(1, X) %*% pi_s_coefficient))
  
  pi_strat <- predict(pi_s_reg, newdata = temp[, !names(temp) %in% c("Y", "A")], type="response")
  
  temp$pi_strat <- (1 - pi_strat) / pi_strat
  
  # decompose in strata 
  if (bin == "quantile"){
    temp$strata <- as.numeric(cut_number(temp$pi_strat, nb_strat))
  } 
  else if (bin == "regular"){
    temp$strata <- as.numeric(cut(temp$pi_strat, nb_strat))
  }
  else {
    print("Wrong bin mentioned")
    break
  }
  
  
  rct <- temp[temp$V ==1,]
  m <- nrow(temp[temp$V == 0,])
  tau_strat <- 0
  
  
  for (s in unique(rct$strata)){
    # compute strata ate
    strata_ate <- mean(rct[rct$strata == s & rct$A == 1, "Y"]) - mean(rct[rct$strata == s & rct$A == 0, "Y"])
    weigth <- nrow(temp[temp$V ==0 & temp$strata == s, ]) / m
    tau_strat <- tau_strat + weigth*strata_ate
    
  }
  
  return(tau_strat)
}

# G-formula
compute_gformula.split_testing <- function(DF){
  
  # colnames(DF)[grep("^X", colnames(DF))] <- paste0(colnames(DF)[grep("^X", colnames(DF))], "a")
  # colnames(DF)[grep("^(X2)", colnames(DF))] <- "race"
  
  
  mu_1 <- lm(Y ~., data = DF[DF$V == 1 & DF$A == 1, !names(DF) %in% c("V", "A")])
  mu_0 <- lm(Y ~., data = DF[DF$V == 1 & DF$A == 0, !names(DF) %in% c("V", "A")])
  
  newdata <- DF[DF$V == 0, !names(DF) %in% c("V", "A")]
  newdata.mu_1 <- remove_missing_levels.fixed2(fit = mu_1, test_data = newdata)
  newdata.mu_0 <- remove_missing_levels.fixed2(fit = mu_0, test_data = newdata)
  
  mu_1_predict <- predict.lm(mu_1, newdata = newdata.mu_1)
  mu_0_predict <- predict.lm(mu_0, newdata = newdata.mu_0)
  
  tau_hat_gformula <- mean(mu_1_predict, na.rm = TRUE) - mean(mu_0_predict, na.rm = TRUE)
  
  return(tau_hat_gformula)  
}

# AIPSW
compute_aipsw.split_testing <- function(DF, p, normalized = FALSE) {
  
  N <- nrow(DF)
  n <- nrow(DF[DF$V ==1, ])
  m <- nrow(DF[DF$V ==0, ])
  
  temp <- DF
  
  ### Outcome modeling
  mu_1 <- lm(Y ~., data = temp[temp$V == 1 & temp$A == 1, !names(temp) %in% c("V", "A")])
  mu_0 <- lm(Y ~., data = temp[temp$V == 1 & temp$A == 0, !names(temp) %in% c("V", "A")])
  
  # RWE estimation
  newdata <- temp[temp$V == 0, !names(temp) %in% c("V", "A")]
  newdata.mu_1 <- remove_missing_levels.fixed2(fit = mu_1, test_data = newdata)
  newdata.mu_0 <- remove_missing_levels.fixed2(fit = mu_0, test_data = newdata)
  
  mu_1_predict <- predict.lm(mu_1, newdata = newdata.mu_1)
  mu_0_predict <- predict.lm(mu_0, newdata = newdata.mu_0)
  tau_gformula <- mean(mu_1_predict, na.rm = TRUE) - mean(mu_0_predict, na.rm = TRUE)
  
  ### IPSW part
  # Estimation of P(V = 1 | X)
  # p <-- P(V = 1 | X) 
  # with logistic regression
  p.fit  <- glm(V ~., family = binomial("logit"), data = temp[, !names(temp) %in% c("A", "Y")])
  p <- predict(p.fit, type = "response", newdata = temp)
  
  # Store odds
  temp$odds <- ((1 - p)/p)
  
  #keep only rct
  temp <- temp[temp$V == 1,]
  
  newdata <- temp[, !names(temp) %in% c("V", "A")]
  newdata.mu_11 <- remove_missing_levels.fixed2(fit = mu_1, test_data = newdata)
  newdata.mu_10 <- remove_missing_levels.fixed2(fit = mu_0, test_data = newdata)
  
  temp$mu_11 <- predict.lm(mu_1, newdata = newdata.mu_11)
  temp$mu_10 <- predict.lm(mu_0, newdata = newdata.mu_10)
  
  if (normalized == FALSE){
    tau_ipsw <- (2/m)*with(temp, sum(odds*A*(Y - mu_11) - odds*(1-A)*(Y - mu_10), na.rm = TRUE))  
  } else {
    tau_ipsw <- with(temp, sum(odds*A*(Y - mu_11)/sum(odds*A) - odds*(1-A)*(Y - mu_10)/sum(odds*(1-A)), na.rm = TRUE))
  }
  
  tau_aipsw <- tau_ipsw + tau_gformula
  return(tau_aipsw)
}


# Dong's transformation to mispecify
transform_X_star <-function(DF){
  DF_transformed <- DF
  DF_transformed$X1 <- exp(DF$X1 / 3)
  DF_transformed$X2 <- DF$X2 / (1+exp(DF$X1)) + 10
  DF_transformed$X3 <- DF$X1 * DF$X3 / 25 + 0.6
  DF_transformed$X4 <- DF$X1 + DF$X4 + 20
  matrix_transformed <- as.matrix(DF_transformed[, c("X1", "X2", "X3", "X4")])
  matrix_transformed <- scale(matrix_transformed, center = c(1,1,1,1))
  return(matrix_transformed)
}

# simulation with continuous outcome
simulate_continuous <- function(n = 1000, m = 49000, p = 4, mu = rep(1, p), Sigma = diag(p), bs = c(-0.5, -0.3, -0.5, -0.4), bs0 = -2.5, beta = c(27.4, 13.7, 13.7, 13.7), b0 = - 100, sigma = 2, misRCT = "correct", misoutcome = "correct", Nested = FALSE) {
  
  # Target population generation 
  covariates <- mvrnorm(n = 50*n, mu, Sigma, tol = 1e-06, empirical = FALSE) # 50*n is roughly the initial population size necessary to have the n
  DF <- as.data.frame(covariates)
  names(DF) <- paste("X", 1:p, sep = "")
  covariates_names <- names(DF)
  
  # RCT probability to sample according to model
  if (misRCT == "correct"){
    etas <- as.vector(covariates %*% bs + bs0)
    
  } else if (misRCT == "exponential") {
    
    # RCT misspecification with exp on all covariates
    etas <- as.vector(exp(covariates) %*% bs + bs0 + 3) # 3 was found manually to keep same proportion m and n
    
  } else if (misRCT == "Partial_X2only"){
    
    # partial misspecification with only X2 affected
    DF_mis <- DF
    DF_mis$X2 <- exp(DF_mis$X2)
    mis_covariatesX2 <- as.matrix(DF_mis[, c("X1", "X2", "X3", "X4")])
    etas <- as.vector(mis_covariatesX2 %*% bs + bs0 + 0.1)
    
  } else if (misRCT == "Partial_X1only"){
    
    # partial misspecification with only X1 affected
    DF_mis <- DF
    DF_mis$X1 <- exp(DF_mis$X1)
    mis_covariatesX1 <- as.matrix(DF_mis[, c("X1", "X2", "X3", "X4")])
    etas <- as.vector(mis_covariatesX1 %*% bs + bs0 + 0.1)
    
  }
  else if(misRCT == "dong"){
    
    miscovariatesDong <- transform_X_star(DF)
    etas <- as.vector (miscovariatesDong %*% bs + bs0)
    
  } else if (misRCT == "strongbias"){
    bs = c(-1.5, -0.3, -0.5, -0.4)
    etas <- as.vector (covariates %*% bs + bs0)
  }  else {
    
    print("Error in RCT specification arguments.")
    break
    
  }
  
  ps = 1 / (1 + exp(-etas))
  DF$ps <- ps
  
  # from probability to RCT indicator
  RCT_indicator <- rbinom(length(ps), 1, as.vector(ps))
  DF$V <- RCT_indicator 
  
  # random treatment assignement within the RCT
  DF$A <- ifelse(DF$V == 1, rbinom(nrow(DF), 1, 0.5), NA)
  
  # keep only interesting variables
  DF <- DF[, c(covariates_names, "A", "V")]
  
  if (!Nested) {
    
    # drop other data
    DF_rct <- DF[DF$V == 1,] 
    
    # generate new observational data
    covariates_rwe <- mvrnorm(n = m, mu, Sigma, tol = 1e-06, empirical = FALSE) 
    DF_rwe <- as.data.frame(covariates_rwe)
    names(DF_rwe) <- paste("X", 1:p, sep = "")
    DF_rwe$V <- rep(0, m)
    DF_rwe$A <- rep(NA, m)
    
  } else {
    
    #here we need to drop values such that the final data set contains m observational values and n RCT values.
    DF_rct <- DF[DF$V == 1,]
    DF_rwe <- DF[DF$V == 0,]
  }
  
  # stack RCT and RWE
  DF <- rbind(DF_rct, DF_rwe)
  
  # reset row number
  rownames(DF) <- 1:nrow(DF)
  
  # compute Y  
  if (misoutcome == "correct"){
    
    error = rnorm(n = nrow(DF), mean = 0, sd = sigma)
    DF$Y = b0 + beta[1]*(DF$A == 1)*DF$X1 + beta[2]*DF$X2 + beta[3]*DF$X3 +
      beta[4]*DF$X4 + error
  } 
  else if (misoutcome == "+a"){
    error = rnorm(n = nrow(DF), mean = 0, sd = sigma)
    DF$Y = b0 + DF$X1 + beta[2]*DF$X2 + beta[3]*DF$X3 +
      beta[4]*DF$X4 + error + beta[1]*(DF$A == 1)
  }
  else if (misoutcome == "wrong")
  {
    error = rnorm(n = nrow(DF), mean = 0, sd = sigma)
    DF$Y = b0 + beta[1]*(DF$A == 1)*DF$X1*DF$X2 + beta[2]*DF$X2 + beta[3]*DF$X3 +
      beta[4]*DF$X4 + error
  } 
  else if (misoutcome == "dong") {
    miscovariatesDong <- transform_X_star(DF)
    DF_Xstar <- as.data.frame(miscovariatesDong)
    names(DF_Xstar) <- paste("X", 1:p, sep = "")
    error = rnorm(n = nrow(DF), mean = 0, sd = sigma)
    DF$Y = b0 + beta[1]*(DF$A == 1)*DF_Xstar$X1 + beta[2]*DF_Xstar$X2 + beta[3]*DF_Xstar$X3 + beta[4]*DF_Xstar$X4 + error
  }
  else {
    print("Parameters misoutcome is badly specified")
    break
  }
  return(DF)
}


# Function that launches rep times the simulation and returns a dataframe with results
compute_estimators_and_store <- function(rep, misoutcome = "correct", misRCT = "correct", N = 50000, m = 49000, n = 1000){
  
  rct_ate <- c()
  ipsw <- c()
  ipsw_norm <- c()
  strat_10 <- c()
  gformula <- c()
  cw <- c()
  aipsw <- c()
  CW <- c()
  ACW <- c()
  
  for (i in 1:rep){
    
    DF <- simulate_continuous(misoutcome = misoutcome, misRCT = misRCT, m = m, n = n)
    
    # naive estimator
    rct_ate <- c(rct_ate, mean(DF[DF$A == 1 & DF$V == 1, "Y"]) - mean(DF[DF$A == 0  & DF$V == 1, "Y"]))
    
    #ispw
    ipsw  <- c(ipsw, compute_ipsw(DF, normalized = F))
    ipsw_norm <- c(ipsw_norm, compute_ipsw(DF, normalized = T))
    
    #strat
    strat_10 <- c(strat_10, compute_stratification(DF, 10))
    
    #gformula
    gformula <- c(gformula, compute_gformula(DF))
    
    #aipsw
    aipsw <- c(aipsw, compute_aipsw(DF))
    
    # cw and acw
    ajustment.set = c("X1", "X2", "X3", "X4")
    calibrations <- genRCT(Y.trial = DF[DF$V == 1, "Y"], 
                           X.trial = DF[DF$V == 1, c("X1", "X2", "X3", "X4")],
                           A.trial = DF[DF$V == 1, "A"],
                           Y.rwe = NULL,
                           X.rwe = DF[DF$V == 0, c("X1", "X2", "X3", "X4")],
                           A.rwe = NULL,
                           family = "gaussian",
                           estimators = c("CW", "ACW-t"), 
                           inference =  FALSE,
                           plot.boot = FALSE,
                           verbose = FALSE)
    
    CW <- c(CW, calibrations$CW)
    ACW <- c(ACW, calibrations$`ACW-t`)
  }
  
  results <- data.frame("RCT" = rct_ate,
                        "IPSW" = ipsw,
                        "IPSW norm" = ipsw_norm,
                        "Stratification n=10" = strat_10,
                        "G-formula" = gformula,
                        "AIPSW" = aipsw,
                        "CW" = CW,
                        "ACW" = ACW) 
}

# create synthetic STAR combined dataset
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
  
  combined_data <- combined_data %>%
    transmute(
      id = student_id,
      V = as.numeric(as.factor(study)) - 1L, # V = 1 implies in RCT: V = 0 implies in OS.
      X1 = gender,
      X2 = fct_collapse(
        race,
        OTHER = c("ASIAN", "HISPANIC", "NATIVE AMERICAN", "OTHER"),
        WHITE = "WHITE",
        BLACK = "BLACK"
      ),
      # X2 = race,
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

# repeated simulation (faster using future_lapply)
repsim.STAR.future <- function(rep) {
  # Define a single simulation iteration
  single_sim <- function(i) {
    DF <- MakeSyntheticSTAR(star_students)
    c(
      compute_ipsw(DF, normalized = TRUE, estimation = "logit", covariates = "All"),
      compute_gformula.split_testing(DF),
      compute_aipsw.split_testing(DF, p = 0)
    )
  }
  
  # Use future_lapply for parallel execution
  mat <- future_lapply(1:rep, single_sim, future.seed = TRUE)
  
  # Combine results into a matrix
  mat <- do.call(rbind, mat)
  return(mat)
}

# slower but will align with set.seed outside of the function
repsim.STAR <- function(rep) {
  mat <- c(0,0,0)
  for(i in 1:rep) {
    DF <- MakeSyntheticSTAR(star_students)
    if (i == 1) {
      mat <- c(compute_ipsw(DF, normalized = TRUE, estimation = "logit", covariates = "All"),
               compute_gformula.split_testing(DF),
               compute_aipsw.split_testing(DF, p=0))
    } else {
      mat <- rbind(mat,c(compute_ipsw(DF, normalized = TRUE, estimation = "logit", covariates = "All"),
                         compute_gformula.split_testing(DF),
                         compute_aipsw.split_testing(DF, p=0)))
    }
  }
  return(mat)
}

star_students <- read_sav("lib/STAR_Students.sav")

set.seed(12345) # for reproduceability

estimates <- repsim.STAR.future(1000)
estimates <- setNames(data.frame(estimates), c("IPSW", "Gformula", "AIPSW"))
head(estimates)
summary(estimates)


boxplot(estimates)
abline(h=5, lty = 2, col = "red")

results_long <- melt(estimates, variable.name = "Method", value.name = "ATE")

# Choose a color palette from wesanderson
palette_colors <- wes_palette("Darjeeling1", 3, type = "discrete")

# Create the box plot with wesanderson colors and custom legend
ggplot(results_long, aes(x = ATE, y = Method, fill = Method)) +
  geom_boxplot() +
  # Add a dummy geom for Population ATE line to appear in the legend
  # geom_vline(aes(xintercept = 5, color = "Population ATE"), linetype = "dashed", size = 0.5) + # Adjust 27.0 to your actual population ATE value
  # labs(x = "Estimated ATE", y = "", fill = "Method", color = "") +
  # theme_minimal() +
  # theme(
  #   legend.position = "right",
  #   axis.text.y = element_text(size = 10),
  #   panel.border = element_rect(fill = NA, size = 1)
  # ) +
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

# set.seed(42)
# 
# combined_data <- MakeSyntheticSTAR(star_students)
# 
# DF <- combined_data
# 
# compute_ipsw(combined_data, normalized = TRUE)
# 
# compute_gformula.split_testing(combined_data)
# 
# compute_aipsw.split_testing(combined_data,p =0) # p was unused





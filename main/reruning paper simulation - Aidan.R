library(reshape2)
library(MASS)
library(dplyr)
library(ggplot2)  # Load ggplot2 for cut_number function
library(table1) # table for baseline
library(wesanderson) # colors
library(genRCT) # remotes::install_github("idasomm/genRCT")

# Run the estimator function
set.seed(123)
results <- compute_estimators_and_store(rep = 100, misoutcome = "correct", misRCT = "correct", N = 50000, m = 49000, n = 1000)

# Display the results
head(results)
summary(results)
boxplot(results)



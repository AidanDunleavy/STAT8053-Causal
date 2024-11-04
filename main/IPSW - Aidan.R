combined_data <- readRDS("lib/combined_data_large.rds")
combined_data <- readRDS("lib/combined_data.rds")

combined_data <- combined_data[, !names(combined_data) %in% c("id")]

combined_data[, !names(combined_data) %in% c("A", "Y")]

# Assuming compute_ipsw is already defined as per your initial code

# Calculate IPSW using the logistic regression estimation and normalization
ipsw_result <- compute_ipsw(combined_data, normalized = TRUE, estimation = "logit", covariates = "All")

gform_result <- compute_gformula(combined_data)

aipsw_result <- compute_aipsw(combined_data, p = 0.5) # p is unused variable


# Print the IPSW result
print(ipsw_result)
print(gform_result)
print(aipsw_result)




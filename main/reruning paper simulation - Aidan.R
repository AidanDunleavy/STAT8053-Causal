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

# Assuming 'results' is the data frame with your simulation results
# Reshape the data for ggplot2
results_long <- melt(results, variable.name = "Method", value.name = "ATE")

# Choose a color palette from wesanderson
print(wes_palette(name = "BottleRocket2"))
palette_colors <- c(wes_palette("GrandBudapest1", 4, type = "discrete"), wes_palette("Moonrise3", 4, type = "discrete"))
palette_colors <- palette_colors[1:length(unique(results_long$Method))]

# Create the box plot with wesanderson colors and custom legend
ggplot(results_long, aes(x = ATE, y = Method, fill = Method)) +
  geom_boxplot() +
  # Add a dummy geom for Population ATE line to appear in the legend
  geom_vline(aes(xintercept = 27.0, color = "Population ATE"), linetype = "dashed", size = 0.5) + # Adjust 27.0 to your actual population ATE value
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

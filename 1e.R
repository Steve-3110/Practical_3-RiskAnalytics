# =====================================================================
# Risk Analytics Assignment - Part 1(e)
# Assessing Compare the tails of the densities of the t-distribution and the normal distribution.
# =====================================================================

# Function to install and load packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of required packages
packages <- c("ggplot2")

# Install and load each package
sapply(packages, install_and_load)
# Define the range for the x-axis
x_values <- seq(-4, 4, by = 0.01)

# Calculate the density of the normal and t-distributions
normal_density <- dnorm(x_values, mean = 0, sd = 1)
#t_density <- dt(x_values, df = fit_t$estimate["df"])  # Use the estimated df from the fitted t-distribution

# Create a data frame for ggplot
density_data <- data.frame(
  x = x_values,
  Normal = normal_density
  #T_Distribution = t_density
)

# Plot the densities
ggplot(density_data, aes(x = x)) +
  geom_line(aes(y = Normal, color = "Normal Distribution"), size = 1) +
  #geom_line(aes(y = T_Distribution, color = "t-Distribution"), size = 1) +
  labs(title = "Comparison of Normal and t-Distribution Densities",
       x = "Value",
       y = "Density") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

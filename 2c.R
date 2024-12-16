# =====================================================================
# Risk Analytics Assignment - Part 2(c)
# ARIMA Model Proposal, Selection, and Residual Assessment
# =====================================================================

# -----------------------------
# 1. Load Necessary Libraries
# -----------------------------

# Function to install and load packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    if(!require(package_name, character.only = TRUE)){
      stop(paste("Package", package_name, "could not be loaded"))
    }
  }
}

# List of required packages
packages <- c("ggplot2", "xts", "dplyr", "gridExtra", "forecast", "tseries")

# Install and load each package, suppressing output
invisible(sapply(packages, install_and_load))

# -----------------------------
# 2. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Verify that required columns exist
required_columns <- c("Bitcoin")
missing_columns <- setdiff(required_columns, names(data))
if(length(missing_columns) > 0){
  stop(paste("The following required columns are missing in 'Crypto_data.csv':",
             paste(missing_columns, collapse = ", ")))
}

# -----------------------------
# 3. Define Negative Log Returns Function
# -----------------------------

negative_log_returns <- function(prices) {
  # Ensure the input is numeric
  prices <- as.numeric(prices)

  # Check for non-positive prices to avoid log issues
  if(any(prices <= 0, na.rm = TRUE)) {
    stop("Prices must be positive to compute logarithms.")
  }

  # Compute log returns: log(P_t) - log(P_{t-1})
  log_returns <- diff(log(prices))

  # Compute negative log returns
  neg_log_returns <- -log_returns

  return(neg_log_returns)
}

# -----------------------------
# 4. Compute Negative Log Returns
# -----------------------------

# Extract Bitcoin prices
bitcoin_prices <- data$Bitcoin

# Compute negative log returns
bitcoin_neg_log_returns <- negative_log_returns(bitcoin_prices)

# -----------------------------
# 5. Create Time Series Objects
# -----------------------------

# Assuming data is recorded every minute for 24 hours (1440 minutes)
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")  # Adjust the date as needed
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects for raw prices and negative log returns
bitcoin_raw_ts <- xts(bitcoin_prices, order.by = time_sequence)
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])

# -----------------------------
# 6. Create a Directory to Save Results
# -----------------------------

# Define a directory to save plots and results
result_dir <- "plots_part2c"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 7. Visualize ACF and PACF
# -----------------------------

# Plot ACF and PACF for Negative Log Returns

# ACF Plot
acf_plot <- ggAcf(bitcoin_neg_log_ts, lag.max = 30) +
  ggtitle("ACF of Bitcoin Negative Log Returns") +
  theme_bw()

# PACF Plot
pacf_plot <- ggPacf(bitcoin_neg_log_ts, lag.max = 30) +
  ggtitle("PACF of Bitcoin Negative Log Returns") +
  theme_bw()

# Combine ACF and PACF plots side by side
combined_acf_pacf <- gridExtra::grid.arrange(acf_plot, pacf_plot, ncol = 2)

# Save the combined ACF and PACF plots
ggsave(filename = file.path(result_dir, "ACF_PACF_NegLogReturns.png"), plot = combined_acf_pacf, width = 16, height = 8, dpi = 300)
cat("Saved ACF and PACF plots for Negative Log Returns.\n")

# -----------------------------
# 8. Propose ARIMA Models Based on ACF and PACF
# -----------------------------

proposed_model_order <- c(1, 0, 0)
proposed_model <- arima(bitcoin_neg_log_ts, order = proposed_model_order)

# Summarize the proposed model
proposed_model_summary <- summary(proposed_model)
print(proposed_model_summary)

# Save the summary to a text file
sink(file = file.path(result_dir, "Proposed_ARIMA_Model_Summary.txt"))
cat("Proposed ARIMA Model: ARIMA(1,0,0)\n\n")
print(proposed_model_summary)
sink()
cat("Saved Proposed ARIMA Model Summary to 'Proposed_ARIMA_Model_Summary.txt'.\n")

# -----------------------------
# 9. Select ARIMA Model Using auto.arima()
# -----------------------------

# Use auto.arima() to select the best-fitting ARIMA model
auto_model <- auto.arima(bitcoin_neg_log_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE, trace = TRUE)

# Summarize the auto.arima model
auto_model_summary <- summary(auto_model)
print(auto_model_summary)

# Save the summary to a text file
sink(file = file.path(result_dir, "Auto_ARIMA_Model_Summary.txt"))
cat("Auto-selected ARIMA Model using auto.arima():\n\n")
print(auto_model_summary)
sink()
cat("Saved Auto ARIMA Model Summary to 'Auto_ARIMA_Model_Summary.txt'.\n")

# -----------------------------
# 10. Compare Proposed and Auto-selected Models
# -----------------------------

# Create a comparison table
model_comparison <- data.frame(
  Model = c("Proposed ARIMA(1,0,0)", paste("Auto ARIMA(", paste(auto_model$arma[c(1,6,2)], collapse = ","), ")", sep = "")),
  AIC = c(proposed_model$aic, auto_model$aic),
  BIC = c(NA, auto_model$bic),  # ARIMA from arima() does not provide BIC directly
  logLik = c(proposed_model$loglik, auto_model$loglik)
)

# Print the comparison table
print(model_comparison)

# Save the comparison table to a CSV file
write.csv(model_comparison, file = file.path(result_dir, "Model_Comparison.csv"), row.names = FALSE)
cat("Saved Model Comparison to 'Model_Comparison.csv'.\n")

# -----------------------------
# 11. Assess Residuals of the Auto-selected Model
# -----------------------------

# Plot Residuals
residuals_plot <- ggplot(data = data.frame(Residuals = residuals(auto_model)), aes(x = Residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  ggtitle("Histogram and Density of Residuals (Auto ARIMA Model)") +
  xlab("Residuals") +
  theme_bw()

# Save Residuals Plot
ggsave(filename = file.path(result_dir, "Residuals_Histogram_Auto_ARIMA.png"), plot = residuals_plot, width = 8, height = 6, dpi = 300)
cat("Saved Residuals Histogram for Auto ARIMA Model.\n")

# Plot ACF of Residuals
residuals_acf <- ggAcf(residuals(auto_model), lag.max = 30) +
  ggtitle("ACF of Residuals (Auto ARIMA Model)") +
  theme_bw()

# Save ACF of Residuals Plot
ggsave(filename = file.path(result_dir, "Residuals_ACF_Auto_ARIMA.png"), plot = residuals_acf, width = 8, height = 6, dpi = 300)
cat("Saved ACF of Residuals for Auto ARIMA Model.\n")

# Perform Ljung-Box Test on Residuals
ljung_box_residuals <- Box.test(residuals(auto_model), lag = 20, type = "Ljung-Box")

# Print and save the Ljung-Box test results
cat("\nLjung-Box Test for Residuals of Auto ARIMA Model:\n")
print(ljung_box_residuals)

# Save the Ljung-Box test results to a text file
sink(file = file.path(result_dir, "LjungBox_Residuals_Auto_ARIMA.txt"))
cat("Ljung-Box Test for Residuals of Auto ARIMA Model:\n\n")
print(ljung_box_residuals)
sink()
cat("Saved Ljung-Box Test Results for Residuals to 'LjungBox_Residuals_Auto_ARIMA.txt'.\n")

# =====================================================================
# End of Part 2(c) Script
# =====================================================================

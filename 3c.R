# =====================================================================
# Risk Analytics Assignment - Part 3(c)
# Granger Causality Test between Bitcoin and Ethereum Negative Log Returns
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
packages <- c("ggplot2", "xts", "dplyr", "gridExtra", "lmtest", "vars")

# Install and load each package, suppressing output
invisible(sapply(packages, install_and_load))

# -----------------------------
# 2. Suppress the 'lag()' Warning from xts
# -----------------------------

# Suppress the specific warning related to 'lag()' from 'xts'
options(xts.warn_dplyr_breaks_lag = FALSE)

# -----------------------------
# 3. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Verify that required columns exist
required_columns <- c("Bitcoin", "Ethereum")
missing_columns <- setdiff(required_columns, names(data))
if(length(missing_columns) > 0){
  stop(paste("The following required columns are missing in 'Crypto_data.csv':",
             paste(missing_columns, collapse = ", ")))
}

# -----------------------------
# 4. Define Negative Log Returns Function
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
# 5. Compute Negative Log Returns for Bitcoin and Ethereum
# -----------------------------

# Extract Bitcoin and Ethereum prices
bitcoin_prices <- data$Bitcoin
ethereum_prices <- data$Ethereum

# Compute negative log returns
bitcoin_neg_log_returns <- negative_log_returns(bitcoin_prices)
ethereum_neg_log_returns <- negative_log_returns(ethereum_prices)

# -----------------------------
# 6. Create Time Series Objects
# -----------------------------

# Assuming data is recorded every minute
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects for negative log returns
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])
ethereum_neg_log_ts <- xts(ethereum_neg_log_returns, order.by = time_sequence[-1])

# -----------------------------
# 7. Create a Directory to Save Results
# -----------------------------

# Define a directory to save plots and results
result_dir <- "plots_part3c"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 8. Determine Optimal Lag Order
# -----------------------------

# Combine the two series into a data frame for VAR analysis
combined_returns <- na.omit(merge(bitcoin_neg_log_ts, ethereum_neg_log_ts))
colnames(combined_returns) <- c("Bitcoin", "Ethereum")

# Convert to time series format
combined_ts <- ts(combined_returns, frequency = 1)

# Select optimal lag order based on AIC
lag_selection <- VARselect(combined_ts, lag.max = 10, type = "const")

# Extract the suggested lag order
optimal_lag <- lag_selection$selection["AIC(n)"]
cat(paste("Optimal lag order selected based on AIC:", optimal_lag, "\n"))

# -----------------------------
# 9. Perform Granger Causality Tests
# -----------------------------

# Test 1: Does Bitcoin Granger-cause Ethereum?
granger_test_btc_causes_eth <- tryCatch({
  grangertest(Ethereum ~ Bitcoin, order = optimal_lag, data = combined_returns)
}, error = function(e){
  cat("Error in Granger Test (Bitcoin → Ethereum):", e$message, "\n")
  return(NULL)
})

# Test 2: Does Ethereum Granger-cause Bitcoin?
granger_test_eth_causes_btc <- tryCatch({
  grangertest(Bitcoin ~ Ethereum, order = optimal_lag, data = combined_returns)
}, error = function(e){
  cat("Error in Granger Test (Ethereum → Bitcoin):", e$message, "\n")
  return(NULL)
})

# -----------------------------
# 10. Save Granger Test Results
# -----------------------------

# Function to save test results to a text file
save_granger_test_result <- function(test_result, test_name, directory) {
  if(!is.null(test_result)){
    sink(file = file.path(directory, paste0("Granger_Test_Result_", test_name, ".txt")))
    cat(paste("Granger Causality Test:", test_name, "\n\n"))
    print(test_result)
    sink()
    cat(paste("Saved Granger Test Result to 'Granger_Test_Result_", test_name, ".txt'.\n"))
  } else {
    cat(paste("Granger Test", test_name, "is NULL. Skipping saving.\n"))
  }
}

# Save both test results
save_granger_test_result(granger_test_btc_causes_eth, "Bitcoin_causes_Ethereum", result_dir)
save_granger_test_result(granger_test_eth_causes_btc, "Ethereum_causes_Bitcoin", result_dir)

# -----------------------------
# 11. Visualize Granger Causality Results (Impulse Response Functions)
# -----------------------------

# Fit a VAR model with the optimal lag
var_model <- tryCatch({
  VAR(combined_ts, p = optimal_lag, type = "const")
}, error = function(e){
  cat("Error in fitting VAR model:", e$message, "\n")
  return(NULL)
})

if(!is.null(var_model)){
  # Plot Impulse Response Functions (IRF) for Bitcoin -> Ethereum
  irf_btc_eth <- irf(var_model, impulse = "Bitcoin", response = "Ethereum",
                    n.ahead = 20, boot = TRUE, ci = 0.95)

  # Save IRF plot for Bitcoin -> Ethereum
  png(filename = file.path(result_dir, "IRF_BTC_to_ETH.png"), width = 800, height = 600)
  plot(irf_btc_eth, main = "Impulse Response: Bitcoin to Ethereum")
  dev.off()
  cat("Saved Impulse Response Function Plot to 'IRF_BTC_to_ETH.png'.\n")

  # Plot Impulse Response Functions (IRF) for Ethereum -> Bitcoin
  irf_eth_btc <- irf(var_model, impulse = "Ethereum", response = "Bitcoin",
                    n.ahead = 20, boot = TRUE, ci = 0.95)

  # Save IRF plot for Ethereum -> Bitcoin
  png(filename = file.path(result_dir, "IRF_ETH_to_BTC.png"), width = 800, height = 600)
  plot(irf_eth_btc, main = "Impulse Response: Ethereum to Bitcoin")
  dev.off()
  cat("Saved Impulse Response Function Plot to 'IRF_ETH_to_BTC.png'.\n")
}

# =====================================================================
# End of Part 3(c) Script
# =====================================================================

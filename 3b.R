# =====================================================================
# Risk Analytics Assignment - Part 3(b)
# Cross-Correlation Function (CCF) Analysis between Bitcoin and Ethereum Negative Log Returns
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
packages <- c("ggplot2", "xts", "dplyr", "gridExtra")

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
result_dir <- "plots_part3b"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 8. Perform Cross-Correlation Function (CCF) Analysis
# -----------------------------

# Ensure both series have the same length
min_length <- min(length(bitcoin_neg_log_ts), length(ethereum_neg_log_ts))
bitcoin_neg_log_ts <- bitcoin_neg_log_ts[1:min_length]
ethereum_neg_log_ts <- ethereum_neg_log_ts[1:min_length]

# Convert xts objects to numeric vectors
btc_returns <- as.numeric(bitcoin_neg_log_ts)
eth_returns <- as.numeric(ethereum_neg_log_ts)

# Compute and plot CCF
ccf_result <- ccf(btc_returns, eth_returns, lag.max = 50, plot = FALSE)

# Create a data frame from CCF results for plotting with ggplot2
ccf_df <- data.frame(
  Lag = ccf_result$lag,
  CCF = ccf_result$acf
)

# Generate CCF plot using ggplot2
ccf_plot <- ggplot(ccf_df, aes(x = Lag, y = CCF)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = c(2/sqrt(length(btc_returns)), -2/sqrt(length(btc_returns))),
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  ggtitle("Cross-Correlation Function (CCF) between Bitcoin and Ethereum Negative Log Returns") +
  xlab("Lag (minutes)") +
  ylab("CCF") +
  theme_bw()

# Save the CCF plot
ggsave(filename = file.path(result_dir, "CCF_BTC_ETH_NegLogReturns.png"),
       plot = ccf_plot, width = 10, height = 6, dpi = 300)
cat("Saved CCF Plot to 'CCF_BTC_ETH_NegLogReturns.png'.\n")

# -----------------------------
# 9. Save CCF Results to a CSV File
# -----------------------------

# Save the CCF data frame to a CSV file
write.csv(ccf_df, file = file.path(result_dir, "CCF_BTC_ETH_NegLogReturns.csv"), row.names = FALSE)
cat("Saved CCF Data to 'CCF_BTC_ETH_NegLogReturns.csv'.\n")

# =====================================================================
# End of Part 3(b) Script
# =====================================================================

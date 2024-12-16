# =====================================================================
# Risk Analytics Assignment - Part 1(b)
# Transforming Bitcoin and Ethereum Prices into Negative Log Returns
# =====================================================================

# -----------------------------
# 1. Load Necessary Libraries
# -----------------------------

# Function to install and load packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of required packages
packages <- c("tseries", "ggplot2", "xts", "dplyr", "forecast")

# Install and load each package
sapply(packages, install_and_load)

# -----------------------------
# 2. Suppress the 'lag()' Warning from xts
# -----------------------------

# Suppress the specific warning related to 'lag()' from 'xts'
options(xts.warn_dplyr_breaks_lag = FALSE)

# -----------------------------
# 3. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
# Assumes 'Crypto_data.csv' is in the working directory
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

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
# 5. Compute Negative Log Returns
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

# Assuming data is recorded every minute for 24 hours (1440 minutes)
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects for negative log returns
# Note: Negative log returns have one less observation than the original prices
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])
ethereum_neg_log_ts <- xts(ethereum_neg_log_returns, order.by = time_sequence[-1])

# -----------------------------
# 7. Plot Negative Log Returns
# -----------------------------

# Convert xts objects to data frames for ggplot2
neg_log_df <- data.frame(
  Time = index(bitcoin_neg_log_ts),
  Bitcoin_Neg_Log_Returns = coredata(bitcoin_neg_log_ts),
  Ethereum_Neg_Log_Returns = coredata(ethereum_neg_log_ts)
)

# Define a directory to save plots
plot_dir <- "plots_part1b"
if(!dir.exists(plot_dir)){
  dir.create(plot_dir)
  cat("Created directory for plots:", plot_dir, "\n")
} else {
  cat("Saving plots to directory:", plot_dir, "\n")
}

# Function to save plots
save_plot <- function(plot, filename) {
  ggsave(filename = file.path(plot_dir, filename), plot = plot, width = 10, height = 6, bg = "white")
}

# Plot Bitcoin Negative Log Returns
plot_btc_neg_log <- ggplot(neg_log_df, aes(x = Time, y = Bitcoin_Neg_Log_Returns)) +
  geom_line(color = "red", linewidth = 0.5) +
  ggtitle("Bitcoin Negative Log Returns Over Time") +
  xlab("Time") +
  ylab("Negative Log Return") +
  theme_bw()  # Changed to theme_bw() for white background

save_plot(plot_btc_neg_log, "Bitcoin_Negative_Log_Returns.png")
cat("Saved plot: Bitcoin_Negative_Log_Returns.png\n")

# Plot Ethereum Negative Log Returns
plot_eth_neg_log <- ggplot(neg_log_df, aes(x = Time, y = Ethereum_Neg_Log_Returns)) +
  geom_line(color = "blue", linewidth = 0.5) +
  ggtitle("Ethereum Negative Log Returns Over Time") +
  xlab("Time") +
  ylab("Negative Log Return") +
  theme_bw()  # Changed to theme_bw() for white background

save_plot(plot_eth_neg_log, "Ethereum_Negative_Log_Returns.png")
cat("Saved plot: Ethereum_Negative_Log_Returns.png\n")

# -----------------------------
# 8. Combined Plot for Comparison
# -----------------------------

combined_plot <- ggplot(neg_log_df, aes(x = Time)) +
  geom_line(aes(y = Bitcoin_Neg_Log_Returns, color = "Bitcoin"), linewidth = 0.5) +
  geom_line(aes(y = Ethereum_Neg_Log_Returns, color = "Ethereum"), linewidth = 0.5) +
  ggtitle("Bitcoin vs Ethereum Negative Log Returns Over Time") +
  xlab("Time") +
  ylab("Negative Log Return") +
  scale_color_manual(name = "Cryptocurrency", values = c("Bitcoin" = "red", "Ethereum" = "blue")) +
  theme_bw()  # Changed to theme_bw() for white background

save_plot(combined_plot, "Bitcoin_vs_Ethereum_Negative_Log_Returns.png")
cat("Saved plot: Bitcoin_vs_Ethereum_Negative_Log_Returns.png\n")

# -----------------------------
# 9. Assess Stationarity
# -----------------------------

# Function to assess stationarity using plots and ADF test
assess_stationarity <- function(time_series, crypto_name) {
  # Define file paths for plots
  ts_plot_file <- file.path(plot_dir, paste0(crypto_name, "_Negative_Log_Returns_TS.png"))
  acf_plot_file <- file.path(plot_dir, paste0(crypto_name, "_Negative_Log_Returns_ACF.png"))

  # Plot the time series and save
  ts_plot <- autoplot(time_series) +
    ggtitle(paste(crypto_name, "Negative Log Returns Over Time")) +
    xlab("Time") +
    ylab("Negative Log Return") +
    theme_bw()}

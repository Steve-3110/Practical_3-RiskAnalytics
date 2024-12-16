# =====================================================================
# Risk Analytics Assignment - Part 2(e)
# Two-Step Approach: ARIMA Modeling Followed by GARCH(1,1) on Residuals
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
packages <- c("ggplot2", "xts", "dplyr", "gridExtra", "forecast", "fGarch", "tseries")

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
required_columns <- c("Bitcoin")
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
# 5. Compute Negative Log Returns
# -----------------------------

# Extract Bitcoin prices
bitcoin_prices <- data$Bitcoin

# Compute negative log returns
bitcoin_neg_log_returns <- negative_log_returns(bitcoin_prices)

# -----------------------------
# 6. Create Time Series Objects
# -----------------------------

# Assuming data is recorded every minute for 24 hours (1440 minutes)
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects for raw prices and negative log returns
bitcoin_raw_ts <- xts(bitcoin_prices, order.by = time_sequence)
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])

# -----------------------------
# 7. Create a Directory to Save Results
# -----------------------------

# Define a directory to save plots and results
result_dir <- "plots_part2e"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 8. Fit ARIMA Model on Negative Log Returns
# -----------------------------

# Use auto.arima() to select the best-fitting ARIMA model
auto_model <- tryCatch({
  auto.arima(bitcoin_neg_log_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE, trace = TRUE)
}, error = function(e) {
  cat("Error in auto.arima():", e$message, "\n")
  return(NULL)
})

if(!is.null(auto_model)){
  # Summarize the auto.arima model
  auto_model_summary <- summary(auto_model)
  print(auto_model_summary)

  # Save the summary to a text file
  sink(file = file.path(result_dir, "Auto_ARIMA_Model_Summary.txt"))
  cat("Auto-selected ARIMA Model using auto.arima():\n\n")
  print(auto_model_summary)
  sink()
  cat("Saved Auto ARIMA Model Summary to 'Auto_ARIMA_Model_Summary.txt'.\n")
} else {
  stop("auto.arima() failed. Cannot proceed with GARCH modeling.")
}

# -----------------------------
# 9. Extract Residuals from ARIMA Model
# -----------------------------

# Extract residuals from the ARIMA model
arima_residuals <- residuals(auto_model)

# Convert residuals to xts object for consistency
arima_residuals_ts <- xts(arima_residuals, order.by = index(bitcoin_neg_log_ts))

# -----------------------------
# 10. Fit GARCH(1,1) Models on ARIMA Residuals
# -----------------------------

# Fit GARCH(1,1) with Normal Distribution on ARIMA Residuals
garch_norm <- tryCatch({
  garchFit(~ garch(1,1), data = arima_residuals_ts, cond.dist = "norm", trace = FALSE)
}, error = function(e){
  cat("Error in fitting GARCH(1,1) with Normal Distribution:", e$message, "\n")
  return(NULL)
})

# Fit GARCH(1,1) with Standardized t-Distribution on ARIMA Residuals
garch_t <- tryCatch({
  garchFit(~ garch(1,1), data = arima_residuals_ts, cond.dist = "std", trace = FALSE)
}, error = function(e){
  cat("Error in fitting GARCH(1,1) with t-Distribution:", e$message, "\n")
  return(NULL)
})

# -----------------------------
# 11. Summarize and Save GARCH Model Summaries
# -----------------------------

# Function to save model summary to a text file
save_model_summary <- function(model, model_name, directory) {
  if(!is.null(model)){
    sink(file = file.path(directory, paste0(model_name, "_Summary.txt")))
    cat(paste("Model Summary for", model_name, ":\n\n"))
    print(summary(model))
    sink()
    cat(paste("Saved", model_name, "Summary to", paste0(model_name, "_Summary.txt"), "\n"))
  } else {
    cat(paste("Model", model_name, "is NULL. Skipping summary.\n"))
  }
}

# Save summaries
save_model_summary(garch_norm, "GARCH_Normal", result_dir)
save_model_summary(garch_t, "GARCH_t", result_dir)

# -----------------------------
# 12. Plot Conditional Volatility
# -----------------------------

# Function to plot conditional volatility
plot_conditional_volatility <- function(model, model_name, directory) {
  if(!is.null(model)){
    cond_vol <- volatility(model)
    dates <- index(arima_residuals_ts)
    vol_df <- data.frame(Date = dates, Conditional_Volatility = cond_vol)

    vol_plot <- ggplot(vol_df, aes(x = Date, y = Conditional_Volatility)) +
      geom_line(color = "blue") +
      ggtitle(paste("Conditional Volatility -", model_name)) +
      xlab("Date-Time") +
      ylab("Conditional Volatility") +
      theme_bw()

    # Save the plot
    ggsave(filename = file.path(directory, paste0("Conditional_Volatility_", model_name, ".png")),
           plot = vol_plot, width = 16, height = 6, dpi = 300)
    cat(paste("Saved Conditional Volatility plot for", model_name, "\n"))
  } else {
    cat(paste("Model", model_name, "is NULL. Skipping Conditional Volatility plot.\n"))
  }
}

# Plot conditional volatilities
plot_conditional_volatility(garch_norm, "GARCH_Normal", result_dir)
plot_conditional_volatility(garch_t, "GARCH_t", result_dir)

# -----------------------------
# 13. Assess Residuals of GARCH Models
# -----------------------------

# Function to assess residuals
assess_residuals <- function(model, model_name, directory) {
  if(!is.null(model)){
    # Extract standardized residuals
    std_resid <- residuals(model, standardize = TRUE)

    # Convert to data frame for plotting
    resid_df <- data.frame(Date = index(arima_residuals_ts), Residuals = std_resid)

    # Plot Residuals Over Time
    resid_time_plot <- ggplot(resid_df, aes(x = Date, y = Residuals)) +
      geom_line(color = "darkgreen") +
      ggtitle(paste("Standardized Residuals Over Time -", model_name)) +
      xlab("Date-Time") +
      ylab("Standardized Residuals") +
      theme_bw()

    # Plot Histogram and Density
    resid_hist_plot <- ggplot(resid_df, aes(x = Residuals)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
      geom_density(color = "red") +
      ggtitle(paste("Histogram and Density of Residuals -", model_name)) +
      xlab("Standardized Residuals") +
      theme_bw()

    # Plot ACF of Residuals
    resid_acf_plot <- ggAcf(std_resid, lag.max = 30) +
      ggtitle(paste("ACF of Residuals -", model_name)) +
      theme_bw()

    # Combine all plots into one grid
    combined_resid_plots <- gridExtra::grid.arrange(resid_time_plot, resid_hist_plot, resid_acf_plot, ncol = 2)

    # Save the combined residual plots
    ggsave(filename = file.path(directory, paste0("Residuals_Plots_", model_name, ".png")),
           plot = combined_resid_plots, width = 16, height = 12, dpi = 300)
    cat(paste("Saved Residuals plots for", model_name, "\n"))

    # Perform Ljung-Box Test on Residuals
    ljung_box_resid <- Box.test(std_resid, lag = 20, type = "Ljung-Box")

    # Print and save the Ljung-Box test results
    cat(paste("\nLjung-Box Test for Residuals of", model_name, ":\n"))
    print(ljung_box_resid)

    # Save the Ljung-Box test results to a text file
    sink(file = file.path(directory, paste0("LjungBox_Test_Residuals_", model_name, ".txt")))
    cat(paste("Ljung-Box Test for Residuals of", model_name, ":\n\n"))
    print(ljung_box_resid)
    sink()
    cat(paste("Saved Ljung-Box Test Results for Residuals of", model_name, "to 'LjungBox_Test_Residuals_", model_name, ".txt'.\n"))
  } else {
    cat(paste("Model", model_name, "is NULL. Skipping residual assessment.\n"))
  }
}

# Assess residuals for both GARCH models
assess_residuals(garch_norm, "GARCH_Normal", result_dir)
assess_residuals(garch_t, "GARCH_t", result_dir)

# =====================================================================
# End of Part 2(e) Script
# =====================================================================

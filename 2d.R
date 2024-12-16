# =====================================================================
# Risk Analytics Assignment - Part 2(d)
# GARCH Model Fitting with Normal and t-Distributions and Residual Assessment
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
packages <- c("ggplot2", "xts", "dplyr", "gridExtra", "fGarch", "forecast", "tseries")

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
result_dir <- "plots_part2d"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 7. Fit GARCH(1,1) Models
# -----------------------------

# Load the fGarch library
library(fGarch)

# Fit GARCH(1,1) with Normal Distribution
garch_norm <- tryCatch({
  garchFit(~ garch(1,1), data = bitcoin_neg_log_ts, cond.dist = "norm", trace = FALSE)
}, error = function(e){
  cat("Error in fitting GARCH(1,1) with Normal Distribution:", e$message, "\n")
  return(NULL)
})

# Fit GARCH(1,1) with Standardized t-Distribution
garch_t <- tryCatch({
  garchFit(~ garch(1,1), data = bitcoin_neg_log_ts, cond.dist = "std", trace = FALSE)
}, error = function(e){
  cat("Error in fitting GARCH(1,1) with t-Distribution:", e$message, "\n")
  return(NULL)
})

# -----------------------------
# 8. Summarize and Save Model Summaries
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
# 9. Plot Conditional Volatility
# -----------------------------

# Function to plot conditional volatility
plot_conditional_volatility <- function(model, model_name, directory) {
  if(!is.null(model)){
    cond_vol <- volatility(model)
    dates <- index(bitcoin_neg_log_ts)
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
# 10. Assess Residuals
# -----------------------------

# Function to assess residuals
assess_residuals <- function(model, model_name, directory) {
  if(!is.null(model)){
    # Extract standardized residuals
    std_resid <- residuals(model, standardize = TRUE)

    # Convert to data frame for plotting
    resid_df <- data.frame(Date = index(bitcoin_neg_log_ts), Residuals = std_resid)

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

    sink(file = file.path(directory, paste0("LjungBox_Test_Residuals_", model_name, ".txt")))
    cat(paste("Ljung-Box Test for Residuals of", model_name, ":\n\n"))
    print(ljung_box_resid)
    sink()
    cat(paste("Saved Ljung-Box Test Results for Residuals of", model_name, "to 'LjungBox_Test_Residuals_", model_name, ".txt'.\n"))
  } else {
    cat(paste("Model", model_name, "is NULL. Skipping residual assessment.\n"))
  }
}

# Assess residuals for both models
assess_residuals(garch_norm, "GARCH_Normal", result_dir)
assess_residuals(garch_t, "GARCH_t", result_dir)

# -----------------------------
# 11. Compare Model Fits
# -----------------------------

# Function to compare models based on AIC and BIC
compare_models <- function(model1, model2, model1_name, model2_name, directory) {
  if(!is.null(model1) & !is.null(model2)){
    comparison_table <- data.frame(
      Model = c(model1_name, model2_name),
      AIC = c(AIC(model1), AIC(model2)),
      BIC = c(BIC(model1), BIC(model2))
    )

    print(comparison_table)

    # Save the comparison table to a CSV file
    write.csv(comparison_table, file = file.path(directory, "GARCH_Model_Comparison.csv"), row.names = FALSE)
    cat("Saved GARCH Model Comparison to 'GARCH_Model_Comparison.csv'.\n")
  } else {
    cat("One or both models are NULL. Skipping model comparison.\n")
  }
}

# Compare GARCH_Normal and GARCH_t
compare_models(garch_norm, garch_t, "GARCH_Normal", "GARCH_t", result_dir)

# =====================================================================
# End of Part 2(d) Script
# =====================================================================

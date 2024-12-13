# Package loading
library(here)
library(fpp2)
library(dplyr)
library(ismev)
library(tseries)
library(extRemes)
library(lubridate)
library(fitdistrplus)
library(gridExtra)
library(kableExtra)
library(evir)
library(POT)

# Import the data from our .csv
american_airlines_data <- read.csv(here::here("flights_sample_3m_filtered.csv"))

# Convert the date in a date format
american_airlines_data$DATE <- as.Date(american_airlines_data$FL_DATE, format = "%Y-%m-%d")

# Calculate max departure delay per day
daily_max_delay <- american_airlines_data %>%
  group_by(FL_DATE) %>%
  summarise(max_delay = max(DEP_DELAY, na.rm = TRUE))


# Transform the dataframe into a time series
daily_max_delay_ts <- ts(daily_max_delay$max_delay, frequency = 365)

write.csv(daily_max_delay,file='C:/Users/steve/Desktop/Practical_3-RiskAnalytics/bla.csv')


# Visualization of the time series (Change the legend y axis and values x axis)
# Observation of trend lower between 2 and 3, higher after that with peak at 5
# Observation of patterns : not obvious
# Delays are more concentrated around the lower range (0–25 minutes) for most days
# Peak at 5 (see what it means in date to give an explanation)
autoplot(daily_max_delay_ts)

# Histogram with our max daily delay
ggplot(data.frame(daily_max_delay_ts), aes(x = daily_max_delay_ts)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of the max daily delay",
       x = "Daily max delay (min)",
       y = "Frequency") +
  theme_minimal()

# Fit the distribution to asses our value
fit_norm <- fitdist(daily_max_delay$max_delay, "norm")
fit_gamma <- fitdist(daily_max_delay$max_delay, "gamma")

# Plot the density of fitted distributions
plot.legend <- c("Normal", "Gamma")
denscomp(list(fit_norm, fit_gamma), legendtext = plot.legend)

# Summary of the fitted distributions
summary(fit_norm)
summary(fit_gamma)

# Compare models using AIC (or BIC)
AIC(fit_norm)
AIC(fit_gamma)
# gamma distribution fits the best our data on AIC and BIC it's very closed form the normal distribution


# STL plot
autoplot(stl(daily_max_delay_ts, t.window = 18, s.window = "periodic"))
stl_result <- stl(daily_max_delay_ts, t.window = 18, s.window = "periodic")

# Extract Residuals (Remainder Component)
residuals_series <- stl_result$time.series[, "remainder"]

residuals_df <- data.frame(
  FL_DATE = daily_max_delay$FL_DATE,  # Date from the original daily_max_delay data
  residual = as.numeric(residuals_series) # Convert the remainder to numeric
)


# Plot the Residuals
autoplot(residuals_series) + 
  ggtitle("Residuals (Trend and Seasonality Removed)") +
  xlab("Time") +
  ylab("Residual Maximum Delay")

# compare the residuals against the original series:
original_plot <- autoplot(daily_max_delay_ts) + 
  ggtitle("Original Series") +
  xlab("Time") +
  ylab("Maximum Delay")

residuals_plot <- autoplot(residuals_series) + 
  ggtitle("Residuals") +
  xlab("Time") +
  ylab("Residual Maximum Delay")

grid.arrange(original_plot, residuals_plot, ncol = 1)

# Slightly better
acf(residuals_series)


############## Linear Model ###########################

# Switch to a date format
daily_max_delay$FL_DATE <- as.Date(daily_max_delay$FL_DATE, format = "%Y-%m-%d")

# Create a new column with Year and Week Number
daily_max_delay$Year_monthNum <- paste(
  format(daily_max_delay$FL_DATE, "%Y"),  # Extract year
  format(daily_max_delay$FL_DATE, "%m"),  # Extract month number
  sep = "-"
)

# Aggregate data by Year and Month Number to calculate monthly maximum delays
monthly_max_delay <- daily_max_delay %>%
  group_by(Year_monthNum) %>%
  summarize(max_delay = max(max_delay))  # Get monthly max delay


monthly_max_delay <- monthly_max_delay %>%
  mutate(
    Year = as.numeric(sub("-.*", "", Year_monthNum)),  # Extract Year
    Month = as.numeric(sub(".*-", "", Year_monthNum))   # Extract Week
  )

monthly_max_delay$Year_Month_Continuous <- monthly_max_delay$Year + (monthly_max_delay$Month / 12)


# Fit a linear model for the weekly maximum delay
Year_Month_model <- lm(max_delay ~ Year_Month_Continuous, data = monthly_max_delay)


last_month_continuous <- max(monthly_max_delay$Year_Month_Continuous)


# Predict the delay for the next 10 weeks
# Create a new dataframe for future weeks
future_months <- data.frame(
  Year_Month_Continuous = seq(
    from = last_month_continuous, 
    by = 1 / 12,   # Incrément de 1 mois (1/12 d'une année)
    length.out = 4 * 12
  )
)

# Predict values for the next months of the next 4 years
predictions <- predict(Year_Month_model, newdata = future_months, interval = "confidence")

# Combine predictions with the future weeks into a dataframe
predicted_data <- data.frame(future_months, predictions)

# Plot the original weekly data, the linear model, and predictions
ggplot() +
  geom_point(data = monthly_max_delay, aes(x = Year_Month_Continuous, y = max_delay), color = "blue", size = 2) +
  geom_line(data = monthly_max_delay, aes(x = Year_Month_Continuous, y = max_delay), color = "blue", size = 1) +
  geom_line(data = predicted_data, aes(x = Year_Month_Continuous, y = fit), color = "red", linetype = "dashed") +
  geom_ribbon(data = predicted_data, aes(x = Year_Month_Continuous, ymin = lwr, ymax = upr), 
              fill = "grey80", alpha = 0.5) +
  labs(
    title = "Daily Maximum Delay with Linear Model Predictions",
    x = "Year",
    y = "Maximum Delay (minutes)"
  ) +
  theme_minimal()

# The linear model doesn't capture any variances among our delay values



################ GEV Model #####################

# Convert our daily max observation to a weekly max observation
# Ensure the date column is in Date format
daily_max_delay$FL_DATE <- as.Date(daily_max_delay$FL_DATE, format = "%Y-%m-%d")

# Add columns for year and week number
daily_max_delay <- daily_max_delay %>%
  mutate(
    Year = year(FL_DATE),       # Extract year
    Week = week(FL_DATE)        # Extract week of the year
  )

# Group by year and week, then calculate the maximum value for each group
weekly_max_delay <- daily_max_delay %>%
  group_by(Year, Week) %>%
  summarize(max_delay = max(max_delay, na.rm = TRUE), .groups = "drop")


# Convert weekly maximum delay data into a time series
weekly_max_delay_ts <- ts(weekly_max_delay$max_delay, start = c(min(weekly_max_delay$Year), min(weekly_max_delay$Week)), frequency = 52.18)

# Apply STL decomposition
stl_result_weekly <- stl(weekly_max_delay_ts, t.window = 13, s.window = "periodic")

# Plot the STL decomposition
autoplot(stl_result_weekly) +
  labs(title = "STL Decomposition of Weekly Maximum Delays",
       x = "Time",
       y = "Maximum Delay (minutes)") +
  theme_minimal()

# Extract residuals (remainder component)
residuals_weekly <- stl_result_weekly$time.series[, "remainder"]

# Plot residuals
autoplot(residuals_weekly) +
  labs(title = "Residuals from STL Decomposition of Weekly Maximum Delays",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

# Convert weekly residuals to a data frame
residuals_weekly_df <- data.frame(
  Week = seq_along(residuals_weekly),  # Create a sequential index for weeks
  Residual = as.numeric(residuals_weekly)  # Convert residuals to numeric
)



# Fit the GEV model with constant parameters on our residual series
gev_const <- fevd(residuals_weekly_df$Residual, type = "GEV")


# Show the result of the modelling
plot(gev_const)

# Extract summaries for the model
summary_const <- summary(gev_const)

# Extract AIC and BIC values
AIC_const <- summary_const$aic
BIC_const <- summary_const$bic

# Print AIC and BIC for comparison
cat("Constant GEV Model - AIC:", AIC_const, "BIC:", BIC_const, "\n")






























# Extract the GEV parameters
location <- gev_const$results$par[1]  # Location parameter
scale <- gev_const$results$par[2]     # Scale parameter
shape <- gev_const$results$par[3]     # Shape parameter

# Define the return period
T_values <- c(1, 2, 5)  # In years
T_weeks <- T_values * 52.18  # Convert into weeks (since we are working with weekly data)

# Calculate the return level using the GEV formula
return_levels <- sapply(T_weeks, function(T) {
  if (shape != 0) {
    location + (scale / shape) * ((-log(1 - 1 / T))^(-shape) - 1)
  } else {
    location - scale * log(-log(1 - 1 / T))
  }
})
names(return_levels) <- paste0(T_values, "-year")

# Create a sequence of weeks matching the weekly dataset
weekly_date_range <- seq(from = min(weekly_max_delay$Year) + (min(weekly_max_delay$Week) / 52.18),
                         to = max(weekly_max_delay$Year) + (max(weekly_max_delay$Week) / 52.18),
                         length.out = nrow(weekly_max_delay))

# Extract trend and seasonal components for each week
trend_component_weekly <- stl_result_weekly$time.series[, "trend"]
seasonal_component_weekly <- stl_result_weekly$time.series[, "seasonal"]

# Ensure the trend and seasonal components align with the weekly date range
if (length(weekly_date_range) > length(trend_component_weekly)) {
  trend_component_weekly <- c(trend_component_weekly, 
                              rep(mean(trend_component_weekly, na.rm = TRUE), 
                                  length(weekly_date_range) - length(trend_component_weekly)))
  seasonal_component_weekly <- c(seasonal_component_weekly, 
                                 rep(mean(seasonal_component_weekly, na.rm = TRUE), 
                                     length(weekly_date_range) - length(seasonal_component_weekly)))
}

# Calculate weekly return levels by adding trend and seasonality
return_levels_weekly <- lapply(return_levels, function(level) {
  level + trend_component_weekly + seasonal_component_weekly
})

# Convert the list into a data frame for visualization
return_level_weekly_df <- data.frame(
  Week = rep(weekly_date_range, times = length(T_values)),
  Return_Level = unlist(return_levels_weekly),
  T = rep(paste0(T_values, "-year"), each = length(weekly_date_range))
)

# Visualization
ggplot() +
  geom_line(data = weekly_max_delay, aes(x = Year + (Week / 52.18), y = max_delay), color = "blue") +  # Observed delays
  geom_line(data = return_level_weekly_df, aes(x = Week, y = Return_Level, color = T), linetype = "dashed") +
  scale_color_manual(values = c("red", "green", "purple"), name = "Return Period") +
  labs(
    title = "Weekly Return Levels for Maximum Delays",
    x = "Week",
    y = "Delay (minutes)"
  ) +
  theme_minimal()

































# Parameters from the GEV model
mu <- gev_const$results$par["location"]  # Location parameter
sigma <- gev_const$results$par["scale"]   # Scale parameter
xi <- gev_const$results$par["shape"]      # Shape parameter

# Specify the delay time for which we want to calculate the return period

Delay_threshold<- 1000

# Calculate the CDF for the given amount using the GEV distribution
if (xi != 0) {
  # GEV with shape parameter not equal to 0
  F_x <- exp(-((1 + (xi * (Delay_threshold - mu)) / sigma) ^ (-1 / xi)))
} else {
  # GEV with shape parameter equal to 0 (Gumbel distribution)
  F_x <- exp(-exp(-(Delay_threshold - mu) / sigma))
}

# Calculate the probability of exceeding the specified precipitation amount
P_exceed <- 1 - F_x

# Calculate the return period
return_period <- 1 / P_exceed

# Output the result
return_period



# Define the threshold
threshold <- 1500

# Calculate dynamic return levels for each week
return_levels_dynamic <- sapply(seq_along(trend_component_weekly), function(i) {
  # Calculate the dynamic level
  dynamic_level <- threshold + trend_component_weekly[i] + seasonal_component_weekly[i]
  
  # Skip if scale is non-positive or dynamic level leads to invalid calculations
  if (is.na(dynamic_level) || scale <= 0 || (1 + (shape * (dynamic_level - location) / scale)) <= 0) {
    return(NA)  # Return NA for invalid cases
  }
  
  # Calculate the return period using the GEV formula
  if (shape != 0) {
    1 / (1 - exp(-((1 + (shape * (dynamic_level - location) / scale))^(-1 / shape))))
  } else {
    1 / (1 - exp(-exp(-(dynamic_level - location) / scale)))
  }
})

# Combine results into a data frame
dynamic_return_df <- data.frame(
  Week = weekly_date_range,
  Return_Period = return_levels_dynamic
)

# Extract year
dynamic_return_df <- dynamic_return_df %>%
  mutate(Year = floor(Week))

# Filter for relevant years and calculate yearly averages
yearly_avg_return_summary <- dynamic_return_df %>%
  filter(Year %in% c(2019, 2020, 2021, 2022, 2023)) %>%
  group_by(Year) %>%
  summarise(
    Avg_Return_Period = mean(Return_Period, na.rm = TRUE),  # Calculate the yearly average
    .groups = "drop"
  )

# Inspect the summary
print(yearly_avg_return_summary)

# Generate a nicely formatted table
library(kableExtra)
yearly_avg_return_summary %>%
  kable(
    format = "html",
    caption = "Yearly Average Return Period Analysis for a Threshold of 1500 Minutes",
    col.names = c("Year", "Avg Return Period (days)"),
    digits = 2  # Round to 2 decimal places
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" " = 1, "Yearly Return Period Analysis" = 1))




############ Peak over the threshold ######################

# Extract the residuals for stationarity
delay_Res <- residuals_df$residual

#Generate the Mean Residual Life (MRL) plot to determine the threshold
mrlplot(delay_Res, main = "Mean Residual Life Plot for Delays (Residuals)")

#Define the threshold based on the MRL plot
threshold <- 800

# Fit the Generalized Pareto Distribution (GPD) to exceedances
exceedances <- delay_Res[delay_Res > threshold]  # Subtract threshold for GPD fitting
fit_gpd <- fitgpd(exceedances, threshold = 800)

# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))  # Set up a 2x2 plotting area

# Generate the four diagnostic plots
plot(fit_gpd)
par(mfrow = c(1, 1))

# Check the parameter of the fit_gdp model
summary(fit_gpd)

# Visualize the time series with exceedances
residuals_df$Exceeds_Threshold <- ifelse(delay_Res > threshold, TRUE, FALSE)

ggplot(residuals_df, aes(x = FL_DATE, y = residual)) +
  geom_line(color = "blue") +  # Plot residuals
  geom_point(data = subset(residuals_df, Exceeds_Threshold == TRUE), aes(x = FL_DATE, y = residual), 
             color = "red", size = 1.5) +  # Highlight exceedances
  labs(title = "Time Series of Residual Delays with Threshold Exceedances",
       x = "Date", y = "Residual Delay (min)") +
  theme_minimal()


# Extract values
logLik <- fit_gpd$logLik  # Log-likelihood
k <- 2                    # Number of parameters
n <- fit_gpd$nat          # Number of exceedances (data points)

# Calculate AIC and BIC
aic <- 2 * k - 2 * logLik
bic <- k * log(n) - 2 * logLik


# Reapply the trend and seasonal components
mean_trend <- mean(stl_result$time.series[, "trend"], na.rm = TRUE)
mean_seasonal <- mean(stl_result$time.series[, "seasonal"], na.rm = TRUE)

# get the paramter of our model
scale <- fit_gpd$param["scale"]
shape <- fit_gpd$param["shape"]
threshold <- fit_gpd$threshold  # The threshold used in the GPD model


# Add the trend and seasonal components back to interpret return levels on the original scale
return_levels_original <- function(levels_residual) {
  return(levels_residual + mean_trend + mean_seasonal)
}

# Adjust return levels by adding trend and seasonality to base levels
# Define return periods
return_periods <- c(1, 3, 5, 10)  # Return periods in years

# Parameter setup for lambda
total_years <- length(unique(format(daily_max_delay$FL_DATE, "%Y")))
num_exceedances <- length(fit_gpd$data)
lambda <- num_exceedances / total_years

# Calculate base return levels (without trend and seasonality) using GPD
return_levels <- sapply(return_periods, function(T) {
  threshold + (scale / shape) * (((T * lambda) ^ shape) - 1)
})

# Create a sequence of weeks matching the weekly dataset
weekly_date_range <- seq(from = min(weekly_max_delay$Year) + (min(weekly_max_delay$Week) / 52.18),
                         to = max(weekly_max_delay$Year) + (max(weekly_max_delay$Week) / 52.18),
                         length.out = nrow(weekly_max_delay))

# Extract trend and seasonal components for each week from STL decomposition
trend_component_weekly <- stl_result_weekly$time.series[, "trend"]
seasonal_component_weekly <- stl_result_weekly$time.series[, "seasonal"]

# Ensure trend and seasonal components align with the weekly date range
if (length(weekly_date_range) > length(trend_component_weekly)) {
  trend_component_weekly <- c(trend_component_weekly, 
                              rep(mean(trend_component_weekly, na.rm = TRUE), 
                                  length(weekly_date_range) - length(trend_component_weekly)))
  seasonal_component_weekly <- c(seasonal_component_weekly, 
                                 rep(mean(seasonal_component_weekly, na.rm = TRUE), 
                                     length(weekly_date_range) - length(seasonal_component_weekly)))
}

# Adjust return levels by adding trend and seasonality to base levels
return_levels_weekly <- lapply(return_levels, function(level) {
  level + trend_component_weekly + seasonal_component_weekly  # Add trend and seasonality once
})

# Convert the list into a data frame for visualization
return_level_weekly_df <- data.frame(
  Week = rep(weekly_date_range, times = length(return_periods)),
  Return_Level = unlist(return_levels_weekly),
  Return_Period = rep(paste0(return_periods, "-year"), each = length(weekly_date_range))
)

# Visualization: Weekly Return Levels
ggplot() +
  geom_line(data = weekly_max_delay, aes(x = Year + (Week / 52.18), y = max_delay), color = "blue") +  # Observed delays
  geom_line(data = return_level_weekly_df, aes(x = Week, y = Return_Level, color = Return_Period), linetype = "dashed") +
  scale_color_manual(values = c("red", "green", "purple", "orange"), name = "Return Period") +
  labs(
    title = "Weekly Return Levels with Trend and Seasonality",
    x = "Week",
    y = "Delay (minutes)"
  ) +
  theme_minimal() <- lapply(return_levels, function(level) {
    level + trend_component_weekly + seasonal_component_weekly  # Add trend and seasonality once
  })


# Create a nicely formatted table
return_levels_plot %>%
  kbl(
    caption = "Return Levels for Maximum Delays",
    col.names = c("Return Period (Years)", "Return Level (Minutes)"),
    digits = 2,  # Round to 2 decimal places
    align = "c"  # Center align columns
  ) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2C3E50") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, color = "darkblue")

###################### Clustering and Seasonal Variations ##############################

# Reset the threshold as we defined before
threshold = 800

# Check for clustering of extremes
extremal_index <- extremalindex(daily_max_delay$max_delay, threshold = threshold)
cat("Extremal Index:", extremal_index, "\n")

# Declustering
declustered_data <- extRemes::decluster(daily_max_delay$max_delay, threshold = 800)

declustered_data_cleaned <- declustered_data[declustered_data > threshold]

# Convert to a data frame for plotting
declustered_df <- data.frame(
  Index = seq_along(declustered_data_cleaned),
  Max_Delay = declustered_data_cleaned
)

# Plot declustered data
ggplot(declustered_df, aes(x = Index, y = Max_Delay)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
  labs(
    title = "Declustered Daily Maximum Delays",
    x = "Index",
    y = "Maximum Delay (Minutes)"
  ) +
  theme_minimal()


extreme_days <- daily_max_delay$max_delay > threshold

# Calculate the probability
n_extreme_today <- sum(extreme_days[-length(extreme_days)])  # excluding the last day to avoid NA
n_both_extreme <- sum(extreme_days[-1] & extreme_days[-length(extreme_days)])  # tomorrow also extreme

if (n_extreme_today > 0) {
  probability_both_extreme <- n_both_extreme / n_extreme_today
} else {
  probability_both_extreme <- NA  # Avoid division by zero if no extremes today
}

# Print the probability
print(paste("Probability that tomorrow is also extreme given today is extreme:", probability_both_extreme))


extreme_consecutive <- data.frame(
  Today_Extreme = extreme_days[-length(extreme_days)],
  Tomorrow_Extreme = extreme_days[-1]
)

ggplot(extreme_consecutive, aes(x = Today_Extreme, y = Tomorrow_Extreme)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  labs(title = "Consecutive Extreme Events", x = "Today Extreme", y = "Tomorrow Extreme")














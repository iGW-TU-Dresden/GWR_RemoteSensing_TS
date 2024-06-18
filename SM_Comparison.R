# The soil moisture data in layer 2 of SoMo.ml will be compared with Berthelin (20 cm depth) data

# install.packages(c("readr", "dplyr", "ggplot2","signal"))
library(readr)
library(dplyr)
library(ggplot2)
library(signal)
library(lubridate)

# Read data
data <- read.table("C:/Users/Jonas/Documents/TU_Dresden/Masterarbeit/Daten/Daten_MA/Soil_moisture_2_EZG_same_time.csv", sep = ";", header = TRUE, skip = 0, nrows = 2869, fileEncoding = "ISO-8859-1", quote = "")
berthelin_data <- read.table("C:/Users/Jonas/Documents/TU_Dresden/Masterarbeit/Daten/Daten_MA/Berthelin_GWsm.csv", 
                             sep = ";", header = TRUE, skip = 1, nrows = 2869, 
                             fileEncoding = "ISO-8859-1", quote = "")

# Combine data
combined_data <- data.frame(
  date = data$date,
  Berthelin_mean_daily_SM = berthelin_data$Berthelin.mean_daily_SM, 
  Soil_moisture_SoMo_ml = data$SM_Prozent
)

# Convert date to the correct format
combined_data$date <- as.Date(combined_data$date, format = "%d.%m.%Y") 

# Calculate weekly averages
weekly_averages <- combined_data %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  summarise(Berthelin_mean_weekly_SM = mean(Berthelin_mean_daily_SM, na.rm = TRUE),
            Soil_moisture_SoMo_weekly_ml = mean(Soil_moisture_SoMo_ml, na.rm = TRUE))

# Calculate monthly averages
monthly_averages <- combined_data %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(Berthelin_mean_monthly_SM = mean(Berthelin_mean_daily_SM, na.rm = TRUE),
            Soil_moisture_SoMo_monthly_ml = mean(Soil_moisture_SoMo_ml, na.rm = TRUE))

##### CALCULATE CORRELATION OF DAILY, WEEKLY, AND MONTHLY SM #########

# Calculate correlation for daily averages
daily_correlation <- cor.test(combined_data$Berthelin_mean_daily_SM, combined_data$Soil_moisture_SoMo_ml, method = "pearson")

# Calculate correlation for weekly averages
weekly_correlation <- cor.test(weekly_averages$Berthelin_mean_weekly_SM, weekly_averages$Soil_moisture_SoMo_weekly_ml, method = "pearson")

# Calculate correlation for monthly averages
monthly_correlation <- cor.test(monthly_averages$Berthelin_mean_monthly_SM, monthly_averages$Soil_moisture_SoMo_monthly_ml, method = "pearson")

# Print results
print(paste("The correlation between the daily averages is: ", daily_correlation$estimate))
print(paste("The correlation between the weekly averages is: ", weekly_correlation$estimate))
print(paste("The correlation between the monthly averages is: ", monthly_correlation$estimate))

## Calculate statistics ##
# Test for linearity of the data

# Open PDF file
pdf("scatterplot.pdf")

plot(combined_data$Berthelin_mean_daily_SM, combined_data$Soil_moisture_SoMo_ml, 
     xlab = "Berthelin_mean_daily_SM", ylab = "Soil_moisture_SoMo_ml",
     main = "Scatterplot of the two variables")
# Draw linear trendline
abline(lm(combined_data$Soil_moisture_SoMo_ml ~ combined_data$Berthelin_mean_daily_SM), col = "red")

# Close PDF file
dev.off()

# Mean comparison
mean_Berthelin <- mean(combined_data$Berthelin_mean_daily_SM)
mean_SoMo_ml <- mean(combined_data$Soil_moisture_SoMo_ml)

mean_diff <- mean(combined_data$Berthelin_mean_daily_SM) - mean(combined_data$Soil_moisture_SoMo_ml)
print(mean_diff)

# Minima and maxima for both time series
SM_min_berthelin <- min(combined_data$Berthelin_mean_daily_SM)
SM_max_berthelin <- max(combined_data$Berthelin_mean_daily_SM)

SM_min_SoMo_ml <- min(combined_data$Soil_moisture_SoMo_ml)

SM_max_SoMo_ml <- max(combined_data$Soil_moisture_SoMo_ml)

# KGE without shift
correlation <- cor(combined_data$Berthelin_mean_daily_SM, combined_data$Soil_moisture_SoMo_ml, use = "complete.obs")
bias <- mean(combined_data$Berthelin_mean_daily_SM) - mean(combined_data$Soil_moisture_SoMo_ml)
sd_ratio <- sd(combined_data$Berthelin_mean_daily_SM) / sd(combined_data$Soil_moisture_SoMo_ml)
KGE <- 1 - sqrt((correlation - 1)^2 + (bias - 1)^2 + (sd_ratio - 1)^2)

## Determine time lag ##
## Calculate cross-correlation to determine time lag
# cross_correlation <- ccf(combined_data$Berthelin_mean_daily_SM, combined_data$Soil_moisture_SoMo_ml, plot = FALSE)

## Determine index of maximum lag value in positive range
# max_lag_index <- which.max(cross_correlation$acf)

## Determine lag value in positive range
# lag <- cross_correlation$lag[max_lag_index]

## Prepare data (if necessary, trim to same length)
berthelin_data <- combined_data$Berthelin_mean_daily_SM
somo_data <- combined_data$Soil_moisture_SoMo_ml

## Trim data to same length
min_length <- min(length(berthelin_data), length(somo_data))
berthelin_data <- head(berthelin_data, min_length)
somo_data <- head(somo_data, min_length)

## Calculate cross-correlation up to lag of 30
cross_correlation <- ccf(berthelin_data, somo_data, lag.max = 30, plot = FALSE)
max_corr <- max(cross_correlation$acf)
lag <- which(cross_correlation$acf == max_corr) - 1 # Each lag represents one day

# CAUTION: For interpretation of the shift, note that the span = 60 with lag.max.
# ... This means a lag of 30 represents a shift of the data by 0 days

## Plot the cross-correlation
ggplot(data = data.frame(lag = cross_correlation$lag, acf = cross_correlation$acf)) +
  geom_bar(aes(x = lag, y = acf), stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Lag", y = "Cross-correlation", title = "Cross-correlation: Soil Moisture Berthelin / SoMo.ml") +
  theme_minimal()

# Shift data by the calculated time lag (note that the absolute value of lag.max must always be subtracted)
combined_data$Soil_moisture_SoMo_ml_shifted <- lag(combined_data$Soil_moisture_SoMo_ml, n = lag-30)

# Calculate statistics with shifted data
correlation_shifted <- cor(combined_data$Berthelin_mean_daily_SM, combined_data$Soil_moisture_SoMo_ml_shifted, use = "complete.obs")
bias_shifted <- mean(combined_data$Berthelin_mean_daily_SM, na.rm = TRUE) - mean(combined_data$Soil_moisture_SoMo_ml_shifted, na.rm = TRUE)
sd_ratio_shifted <- sd(combined_data$Berthelin_mean_daily_SM, na.rm = TRUE) / sd(combined_data$Soil_moisture_SoMo_ml_shifted, na.rm = TRUE)
KGE_shifted <- 1 - sqrt((correlation_shifted - 1)^2 + (bias_shifted - 1)^2 + (sd_ratio_shifted - 1)^2)

# Compile parameters into a dataframe
parameters <- data.frame(
  Parameter = c("correlation", "bias", "sd_ratio", "KGE", "max_corr", "mean_Berthelin", "mean_SoMo_ml", "mean_diff", "SM_min_berthelin", "SM_max_berthelin", "SM_min_SoMo_ml", "SM_max_SoMo_ml", "lag", "correlation_shifted", "bias_shifted", "sd_ratio_shifted", "KGE_shifted"),
  Value = c(correlation, bias, sd_ratio, KGE, max_corr, mean_Berthelin, mean_SoMo_ml, mean_diff, SM_min_berthelin, SM_max_berthelin, SM_min_SoMo_ml, SM_max_SoMo_ml, lag, correlation_shifted, bias_shifted, sd_ratio_shifted, KGE_shifted)
)

# Write CSV file
write.csv(parameters, file = "C:/Users/Jonas/Documents/TU_Dresden/Masterarbeit/Daten/Daten_MA/SM_Statistik_Layer2_variables.csv", row.names = FALSE)


# Load in the two RCM Models
source("misc-code/downscale-rcms.R")

# Overlay the average peak ----------------------

# Group the data by period and pull out the maximum
# of each period. Then calculate the mean of these peaks
current_max <- current_ds %>%
                        group_by(period) %>%
                            summarise(peak_max = max(swe_preds),
                                        peak_date = find_peak_index(swe_preds),
                                        first_date = find_first(swe_preds),
                                        duration = find_duration(swe_preds))
current_peak_mean <- mean(current_max$peak_max)
last_day <- current_max$first_date + current_max$duration
as.Date("1970-07-01") + mean(last_day)
as.Date("1970-07-01") + mean(current_max$peak_date)
as.Date("1970-07-01") + mean(current_max$first_date)
current_peak_mean


future_max <- future_ds %>%
    group_by(period) %>%
    summarise(peak_max = max(swe_preds),
              peak_date = find_peak_index(swe_preds),
              first_date = find_first(swe_preds),
              duration = find_duration(swe_preds))
future_peak_mean <- mean(future_max$peak_max)
last_day <- future_max$first_date + future_max$duration
as.Date("1970-07-01") + mean(last_day)
as.Date("1970-07-01") + mean(future_max$peak_date)
as.Date("1970-07-01") + mean(future_max$first_date)
future_peak_mean


# Plot the data with the mean peak of each period overlayed
par(mfrow = c(1, 2))
plot(current_ds$date, current_ds$swe_preds, type = "l",
     ylim = c(0, 25), ylab = "Snow Water Equivalent", xlab = "")
abline(h = current_peak_mean, col = "red", lwd = 2)
plot(future_ds$date, future_ds$swe_preds, type = "l",
     ylim = c(0, 25), , xlab = "", ylab = "")
abline(h = future_peak_mean, col = "red", lwd = 2)

# SWE Trajectories Current vs. Future -------------
periods <- unique(current_ds$period)
for (per in periods) {
    temp <- subset(current_ds, period == per)
    preds <- temp$swe_preds
    if (per == periods[1]) {
        dates <- temp$date
        plot(dates, preds, col = sample(colours(), 1), type = "l",
             ylim = c(0, 25), main = "CRCM-GCM Predicted SWE 1971-1998")
    } else {
        if (nrow(temp) < length(dates)) {
            preds <- c(preds, NA)
        }
        lines(dates, preds, col = sample(colours(), 1))
    }
}

# Make the same plot for the future projections
periods <- unique(future_ds$period)
for (per in periods) {
    temp <- subset(future_ds, period == per)
    preds <- temp$swe_preds
    if (per == periods[1]) {
        dates <- temp$date
        plot(dates, preds, col = sample(colours(), 1), type = "l",
             ylim = c(0, 25), main = "CRCM-GCM Predicted SWE 2040-2070")
    } else {
        if (nrow(temp) > length(dates)) {
            preds <- preds[1:365]
        }
        lines(dates, preds, col = sample(colours(), 1))
    }
}

# Mean Trajectory in Both time Periods ---------------------------
par(mfrow = c(1, 1))
dates <- seq(as.Date("1980-01-01"), by = 1, len = 366)
day_month <- format.Date(current_ds$date, "%m%d")
current_ds$day_month <- as.numeric(day_month)

current_day_groups <- current_ds %>%
                group_by(day_month) %>%
                summarise(day_avg = mean(swe_preds))

start_index <- which(current_day_groups$day_month >= 701)
end_index <- which(current_day_groups$day_month < 701)
index <- c(start_index, end_index)
ordered_current_day <- current_day_groups[index, ]
current_day_avg <- ordered_current_day$day_avg
plot(dates, current_day_avg, type = "l", ylim = c(0, 15),
     main = "Mean SWE Trajectory Current and Future Time Periods",
     col = "red", lwd = 2, cex.main = 1.5, cex.lab = 1.4,
     ylab = "Snow Water Equivalent", xlab = "")


day_month <- format.Date(future_ds$date, "%m%d")
future_ds$day_month <- as.numeric(day_month)

future_day_groups <- future_ds %>%
    group_by(day_month) %>%
    summarise(day_avg = mean(swe_preds))

start_index <- which(future_day_groups$day_month >= 701)
end_index <- which(future_day_groups$day_month < 701)
index <- c(start_index, end_index)
ordered_future_day <- future_day_groups[index, ]
future_day_avg <- ordered_future_day$day_avg
lines(dates, future_day_avg, type = "l", ylim = c(0, 15),
      col = "blue", lwd = 2)
legend("topleft", c("1971-1998", "2041-2068"), col = c("red", "blue"), lwd = 3)

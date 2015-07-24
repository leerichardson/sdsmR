g# Cleaning/Tidying the data ----------------------------

# Download the NIWOT dataset from the online link
niwot_url <- "http://www.wcc.nrcs.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/663:CO:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value"
download.file(url = niwot_url, destfile = "/home/lee/Dropbox/work/ncar/data/snotel/niwot")

# Real in the downloaded file
niwot_data <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot_snotel",
                       comment.char = "#", stringsAsFactors = FALSE)
niwot_data$Date <- as.Date(niwot_data$Date)
colnames(niwot_data) <- c("date", "swe", "precip_accum", "temp_max", "temp_min",
                          "temp_avg", "precip_inc")

narcaap_preds <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot_site/niwot_narcaap.csv")
complete_columns <- unlist(lapply(narcaap_preds, function(x) any(!is.na(x))))
narcaap_preds <- narcaap_preds[, complete_columns]
date <- seq(as.Date("1979-01-01"), by = 1, len = nrow(narcaap_preds))
narcaap_preds$date <- date

# Read in the niwot predictor variables
niwot_predictors <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot.csv")
date <- seq(as.Date("1979-01-01"), by = 1, len = nrow(niwot_predictors))
niwot_predictors <- cbind(date, niwot_predictors)

# Merge together the predictor and predictands. And remove
# the two datasets that we merged together!
niwot <- merge(niwot_data, niwot_predictors, by = "date",
               all.x = TRUE, sort = TRUE)
write.csv(niwot[, 2:ncol(niwot)], "/home/lee/Dropbox/work/ncar/data/snotel/niwot.csv", row.names = FALSE)
rm(niwot_data, niwot_predictors)

# Exploratory Analysis -------------------------------------------

# Plot all of the variables over a two year period to see
# their overall relationship
par(mfrow = c(2,3))
plot_index <- 3288:8000
plot_dates <- niwot$date[plot_index]
plot(plot_dates, niwot$swe[plot_index], main = "Snow Water Equivalent")
plot(plot_dates, niwot$precip_accum[plot_index], main = "Precipitation Accumulation")
plot(plot_dates, niwot$precip_inc[plot_index], main = "Precipitation Increment")
plot(plot_dates, niwot$temp_max[plot_index],
     main = "Maximum Temperature", ylim = c(-5, 90))
plot(plot_dates, niwot$temp_avg[plot_index],
     main = "Average Temperature", ylim = c(-5, 90))
plot(plot_dates, niwot$temp_min[plot_index],
     main = "Minimum Temperature", ylim = c(-5, 90))
par(mfrow = c(1, 1))

# Plot just the Snow Trend
plot(plot_dates, niwot$swe[plot_index], main = "Snow Water Equivalent Pattern",
     type = "l", xlab = "", ylab = "Snow Water Equivalent")


# Use the generate_table function which comes with the sdsmR
# package in order to explore the
generate_table("/home/lee/niwot", niwot, y = "swe")

# Look into the relationship between precipitation accumulation
# and snow water equivalent.
plot_index <- 5850:9000
plot(niwot$date[plot_index], niwot$swe[plot_index], cex = .5,
     pch = 16, ylim = c(0, 40), main = "Snow Water Equivalent in Niwot from 1980 - 2015",
     xlab = "", ylab = "Snow Water Equivalent", type = "l")
lines(niwot$date[plot_index], niwot$precip_accum[plot_index],
      col = "blue", lwd = 3)
lines(niwot$date[plot_index], niwot$temp_avg[plot_index],
      col = "red", lwd = .5)

# Look into the relationship with SWE and other promising
# variables
pi <- 1:1000
plot(niwot$date[pi], scale(niwot$swe)[pi], ylim = c(-3, 3), type = "l")
lines(niwot$date[pi], niwot$hgt_150_lag[pi], col = "blue")
lines(niwot$date[pi], niwot$air_300_lag[pi], col = "green")
lines(niwot$date[pi], scale(niwot$temp_avg)[pi], col = "purple")


niwot_08 <- subset(niwot, year %in% c(2008:2009))
plot(niwot_08$date, niwot_08$swe, ylim = c(0, 25), pch = 16, cex = .5,
     main = "Precip Accumulation Less than Snow in 2008",
     xlab = "Time", ylab = "Snow Water Equivalent")
lines(niwot_08$date, niwot_08$precip_accum, col = "blue", lwd = 2)

# Swe Precip general trend
niwot_08 <- subset(niwot, year %in% c(1990:2000))
plot(niwot_08$date, niwot_08$swe, ylim = c(0, 32), pch = 16, cex = .5,
     main = "Relationship between Snow Water Equivalent and Precipitation", xlab = "Time", ylab = "Snow Water Equivalent")
lines(niwot_08$date, niwot_08$precip_accum, col = "blue", lwd = 2)


# # Plot Temperature During Peaks ------------------------------
# # Plot the profile of Temperature during the peak Snowfall in
# # the Niwot data-frame.
# periods <- unique(niwot$period[!is.na(niwot$period)])
# decline_indices <- matrix(NA, nrow = 11, ncol = length(periods))
# for (i in seq_along(periods)) {
#     period_peak <- find_period_peak(dataframe = niwot, per = i, peak_var = "swe")
#     indices <- indices_window(period_peak)
#     decline_indices[, i] <- indices
# }
#
#
# count <- 0
# smooth_temps <- rollmean(x = niwot[, "temp_avg"], n = 3)
# for (i in seq_along(periods)) {
#     plot_index <- decline_indices[, i]
#
#     # Skip if the temperature here has any NA's
#     smooth_temp <- smooth_temps[plot_index]
#     if (sum(is.na(smooth_temp)) > 8) {
#         next
#     }
#
#     print(smooth_temp)
#     count <- count + 1
#     if (count == 1) {
#         plot(1:length(plot_index), smooth_temp, ylim = c(15, 55),
#              type = "l", col = sample(colours()[73:110], 1),
#              main = "Smooth Temperature During Peak SWE", ylab = "Smooth Temperature",
#              xlab = "Day's surrounding Peak SWE")
#     } else {
#         lines(1:length(plot_index), smooth_temp,
#               col = sample(colours()[73:110], 1))
#     }
# }
# abline(v = 6, lwd = 3)
#
# rm(decline_indices, count, i, start_per_25, relevant_months, monthly_means,
#    period_parameters, sparse_preds, peak_months, per, dates, indices, )


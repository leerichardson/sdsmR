# Cleaning/Tidying the data ----------------------------

# Download the NIWOT dataset from the online link
niwot_url <- "http://www.wcc.nrcs.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/663:CO:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value"
download.file(url = niwot_url, destfile = "/home/lee/Dropbox/work/ncar/data/snotel/niwot")

# Real in the downloaded file
niwot_data <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot_snotel",
                       comment.char = "#", stringsAsFactors = FALSE)
niwot_data$Date <- as.Date(niwot_data$Date)
colnames(niwot_data) <- c("date", "swe", "precip_accum", "temp_max", "temp_min",
                          "temp_avg", "precip_inc")

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
plot_index <- 3288:5000
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


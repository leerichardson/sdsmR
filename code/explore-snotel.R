# Download the NIWOT dataset from the online link
niwot_url <- "http://www.wcc.nrcs.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/663:CO:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value"
download.file(niwot_url)

# Real in the downloaded file
niwot_data <- read.csv("data/snotel/niwot", comment.char = "#",
                       stringsAsFactors = FALSE)
niwot_data$Date <- as.Date(niwot_data$Date)


# Plot all of the variables over a two year period.
par(mfrow = c(2,3))
plot_index <- 3288:5000
plot_dates <- niwot_data$Date[plot_index]
plot(plot_dates, niwot_data$Snow.Water.Equivalent..in.[plot_index], main = "Snow Water Equivalent")
plot(plot_dates, niwot_data$Precipitation.Accumulation..in.[plot_index], main = "Precipitation Accumulation")
plot(plot_dates, niwot_data$Precipitation.Increment..in.[plot_index], main = "Precipitation Increment")
plot(plot_dates, niwot_data$Air.Temperature.Maximum..degF.[plot_index],
     main = "Maximum Temperature", ylim = c(-5, 90))
plot(plot_dates, niwot_data$Air.Temperature.Average..degF.[plot_index],
     main = "Average Temperature", ylim = c(-5, 90))
plot(plot_dates, niwot_data$Air.Temperature.Minimum..degF.[plot_index],
     main = "Minimum Temperature", ylim = c(-5, 90))



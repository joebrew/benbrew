install.packages("weatherData")
library(weatherData)

# Get weather for just one day
getWeatherForDate("GNV", "2014-04-14")

# Get weather for a period of time
start <- as.Date("2014-01-01", format = "%Y-%m-%d") # define jan 1 2014 as start
x <- getSummarizedWeather("GNV", start_date = start, 
                                end_date = Sys.Date() - 1,
                                opt_custom_columns = TRUE,
                                custom_columns = c(2,4,20))

# Glimpse at your data
head(x)

# Plot rainfall
plot(x$Date, as.numeric(x$PrecipitationIn), 
     cex = as.numeric(x$PrecipitationIn),
     col = adjustcolor("red", alpha.f=0.4),
     pch = 16)
lines(x$Date, as.numeric(x$PrecipitationIn),
      col = adjustcolor("black", alpha.f = 0.4))

# Plot max temperature
plot(x$Date, x$Max_TemperatureF, 
     col = adjustcolor("darkred", alpha.f = 0.3),
     pch = 16)

# Add min temperature
points(x$Date, x$Min_TemperatureF, 
       col = adjustcolor("darkblue", alpha.f = 0.3),
       pch = 16,
       add = T)

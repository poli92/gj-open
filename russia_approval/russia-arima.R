library(XLConnect)
library(forecast)
library(ggplot2)
library(tseries)

setwd('X:\\Personal Development\\Data Science\\HFC')

# import Russia polling data
df <- readWorksheetFromFile("Russia Approval Data.xlsx", 
                            sheet=1)

df$Period <- as.Date(df$Period)

# Plot the approval ratings
ggplot(df, aes(Period, Approval)) + geom_line() + scale_x_date('month')  + ylab("Russian Government Approval") +
  xlab("")

# Convert approval rates into a time series element starting August 2008 and ending August 2017
myts <- ts(df$Approval, start=c(2012, 8), end=c(2017, 8), frequency=12)

# clean the time series and join it with the existing df
df$clean_Approval <- tsclean(myts)

# Plot the cleaned time series
ggplot() +
  geom_line(data = df, aes(x = Period, y = clean_Approval)) + ylab('Cleaned Approval Rating')

# Add month moving averages
df$quarterly_moving_average <- ma(df$clean_Approval, order = 3)
df$annual_moving_average <- ma(df$clean_Approval, order = 12)

# plot the figures together 
ggplot() +
  geom_line(data = df, aes(x = Period, y = clean_Approval, colour = "Monthly")) +
  geom_line(data = df, aes(x = Period, y = quarterly_moving_average, colour = "3 Month Moving Average")) + 
  geom_line(data = df, aes(x = Period, y = annual_moving_average, colour = "12 Month Moving Average")) + 
  ylab('Aproval Rating')

# Auto-fit an ARIMA model
fit <- auto.arima(df$clean_Approval)

fcast <- forecast(fit, h = 3)

fcast

plot(fcast)

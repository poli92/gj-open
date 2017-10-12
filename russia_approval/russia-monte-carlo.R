library(XLConnect)
library(forecast)

setwd('/Users/josephwood/Dropbox/R/HFC')

# import Russia polling data
df <- readWorksheetFromFile("Russia Approval Data.xlsx", 
                            sheet=1)

df$Period <- as.Date(df$Period)

# Convert approval rates into a time series element starting August 2008 and ending August 2017
myts <- ts(df$Approval, start=c(2012, 8), end=c(2017, 8), frequency=12)

plot(myts)

fit <- stl(myts, s.window="period")
plot(fit)

fit <- auto.arima(myts)
plot(fit)


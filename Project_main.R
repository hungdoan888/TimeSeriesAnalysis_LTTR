# Input Libraries
library(astsa)
library(forecast)
library(fGarch)
library(tseries)
library(rugarch)
library(readxl)

# Load Data
la_data <- read_excel("la_data.xlsx")

# Defining variables
date <- la_data$Date     # Defining date
lttr <- la_data$LA_LTTR  # Defining lttr
gas <- la_data$LA_Gas_Price

# Plot la_data lttr
plot(date, lttr, type = "l", xlab = "Date", ylab = "LTTR")
plot(date, gas,type = "l", xlab = "Date", ylab = "Gas Prices")

# Difference the data
diff_lttr <- diff(lttr)
diff_gas <- diff(gas)
plot.ts( diff_lttr, type = "l" )
plot.ts( diff_gas, type = "l" )

# Plot ccf of non-transformed data
ccf (lttr, gas, lag.max = 100)
lag2.plot(diff_lttr, diff_gas, 8)

# Turning data into time series of period 52
diff_lttr52 <- ts( diff_lttr, frequency=52 )
plot.ts( diff_lttr52)

# Auto Arima to find seasonal effects
auto.arima (diff_lttr52,trace=TRUE)

# Viewing ACF of diff_lttr52
acf(diff_lttr52, 200)
pacf(diff_lttr52, 200)

# SARIMA for yearly including forecast
lttr52 <- ts( lttr, frequency=52 )
fit1 <- sarima(lttr52,1,1,1,0,0,1,52)
sarima.for(lttr52,52,1,1,1,0,0,1,52,plot.all=TRUE)

# Garch fit including forecast
summary(fit2 <- garchFit(~arma(1,2) + garch(1,1),data = diff_lttr52,cond.dist = "std"))
#plot(fit2)
predict(fit2,n.ahead = 52,plot=TRUE,mse="uncond")
AIC_garch = -2*-270.8929 + 2*7
AIC_garch

# Alternative attempts at Forecasting GARCH
'arima1 <- arima(diff_lttr52,order=c(1,0,2))
garch1 <- garch(resid(arima1),order=c(1,1),trace=0)
forecastGARCH(arima1,garch1,r=6,trace=TRUE)
garch3 <- garch(diff_lttr52,order=c(1,0,2))
fit5 <- arima1+garch3
ugarchforecast(fit2)
ugarchfit(diff_lttr52)
fit2
ugarchspec(auto.arima(diff_lttr52))'
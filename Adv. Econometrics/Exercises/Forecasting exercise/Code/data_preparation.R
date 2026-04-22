## Data downloa and preparation
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Università/2. Magistrale/Github/University/Adv. Econometrics/Exercises/Forecasting exercise")

# get yahoo finance data through quantmod
library(quantmod)
library(tidyverse)

# Download daily S&P 500 data
getSymbols("^GSPC", from = "1990-01-01", to = "2026-04-01", src = "yahoo")      #  the ticket is the first argument
str(GSPC)

# now aggregrate monthly NB just closing prices
sp500_monthly <- to.monthly(GSPC, indexAt = 'lastof', OHLC = FALSE)
prices <- sp500_monthly[,"GSPC.Close"]

# computing returns r_t = log(prices[t]/prices[t-1])
returns <- diff(log(prices))
returns <- returns[-1]    # to remove the first NA
dates <- index(returns)   # NB ora e un date format, se lo facessimo dopo la trasformazione 
                          # in vettore numerico avremmo una sequenza da 1:T

returns <- as.numeric(returns)  # converting in a numeric vector for easiest manipulation
T_obs <- length(returns)

# save data locally
write.csv(data.frame(date = dates, log_return = returns), "sp500_monthly_returns.csv", row.names = FALSE)

# first look at data
plot(dates, returns, type = 'l', col = 'blue', xlab = 'Time', ylab = 'log returns', main = 'S&P500 Log-Monthly Returns')
abline(h = 0, lty = 2, col = 'red')



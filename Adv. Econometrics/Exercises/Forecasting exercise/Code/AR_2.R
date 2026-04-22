# Estimating an AR(p), R session
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Università/2. Magistrale/Github/University/Adv. Econometrics/Exercises/Forecasting exercise")

library(readr)
sp500_monthly_returns <- read_csv("Data/sp500_monthly_returns.csv")

## two ways are possible: manual or ar() function

## manual
n <- length(sp500_monthly_returns$log_return)
y <- sp500_monthly_returns$log_return[3:n]
x <- cbind(1, 
           sp500_monthly_returns$log_return[2:(n-1)], 
           sp500_monthly_returns$log_return[1:(n-2)]) 

beta_hat <- solve(t(x) %*% x) %*% (t(x)%*%y)
resid <- y - x %*% beta_hat
sigma2_hat <- sum(resid^2)/(length(y)-ncol(x))

## ar() function
ar2_fit <- ar(sp500_monthly_returns$log_return, aic = FALSE, order.max = 2, method = 'ols', demean = TRUE)

## Mike Yang
## HW1
## Used CPI data from Fred
## Q1
library(readr)
library(tstools)
cpi.raw <- read_csv("CORESTICKM159SFRBATL.csv")
cpi <- ts(cpi.raw[,2], start=c(1968,1), frequency=4)
fit <- tsreg(cpi, lags(cpi,10:14))
trend <- fit$fitted
cpi.both <- ts.combine(cpi, trend)
plot(cpi.both, plot.type="single",
     main="CPI and Its Trend",
     lty=c(1,2))
transitory <- fit$resids
plot(transitory, main="Transitory")

##Q2
##find b with AR(3)
arma30 <- arima(cpi, order=c(3, 0, 0))
## arma30 =  0.7751  -0.0319  0.1134
## nrow(cpi) = 291
b1 <- 0.7751
b2 <- -0.0319
b3 <- 0.1134
e <- rep(0.0, 291)

irf3 <- function(y.last, newinfo) {
  return(c(b1*y.last[1] + b2*y.last[2] +b3*y.last[3], y.last[1], y.last[2]))
}

pull <- function(z) { z[1:3] }
pull(irf3(c(1.0, 1.0, 1.0)))
irf.values <- sapply(irf3, pull)
plot (ts(irf.values), main="IRF")
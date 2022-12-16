## Final Project
## Mike Yang
## Download tools
library(tstools)
library(readr)
library(nleqslv)
library(vars)
library(gmm)

## import data
## I used %change US Unemployment rate, %change in US NET Export and %Change US Import  
ump <- read_csv("UNRATE.csv")
ump <- ts(ump[,2])
plot(ump)
gdp <- read_csv("GDP.csv")
gdp <- ts(gdp[,2])
plot(gdp)
imp <- read_csv("IMPGS.csv")
imp <- ts(imp[,2])
plot(imp)

## checking stationary 
ump.ar1 <- arima(ump,order=c(1,0,0))
gdp.ar1 <- arima(gdp,order=c(1,0,0))
imp.ar1 <- arima(imp,order=c(1,0,0))
## data are stationary

# Estimate the reduced form VAR
rhs <- ts.combine(ump, gdp, imp)
fit.ump <- tsreg(ump, rhs)
fit.gdp <- tsreg(gdp, rhs)
fit.imp <- tsreg(imp, rhs)

# Covariance matrix of the residuals
ump.res <- fit.ump$resids
gdp.res <- fit.gdp$resids
imp.res <- fit.imp$resids
res <- ts.combine(ump.res, gdp.res, imp.res)
cov(res)

##Using recursiveness assumption. (b=c=f=0)
nleq.obj <- function(par) {
  b <- 0
  c <- 0
  d <- par[1]
  f <- 0
  g <- par[2]
  h <- par[3]
  sump <- par[4]
  sgdp <- par[5]
  simp <- par[6]
  dev1 <- sump + (b^2)*sgdp + (c^2)*simp - 7.679250e-29
  dev2 <- d*sump + b*sgdp + c*f*simp + 6.420425e-27
  dev3 <- g*sump + b*h*sgdp + c*simp - 6.709431e-31
  dev4 <- d*sump + sgdp + f^2*simp - 2.745656e-23
  dev5 <- d*sump + h*sgdp + f*simp - 7.616497e-28
  dev6 <- g^2*sump + h^2*sgdp + simp - 2.828570e-30
  return(c(dev1 , dev2 , dev3 , dev4 , dev5 , dev6))
}
nleq.obj(c(0,0,0,0,0,0))
nleqslv(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1), nleq.obj)

varfit <- VAR(ts.combine(ump, gdp, imp), 1)
F <- formF(varfit)
F

nextForecast <- function(lastForecast, newinfo) {
  return(F %*% lastForecast)
}

last(ump)
last(gdp)
last(imp)

forecasts.data <- Reduce(nextForecast, 1:20,
                         init=c(5.55556, -0.77626, -3.53407, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")

## shock on import +1% point on UMP with no direct effct on ump, gdp
forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(5.55556, -0.77626, -2.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")

irf <- counterfactual - nochange
irf
plot(ts(irf))

## shock on import +1% point on GDP with no direct affect form
forecasts.data <- Reduce(nextForecast, 1:20,
                         init=c(5.55556, -0.77626, -3.53407, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[2]},
                    output="numeric")

forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(5.55556, -0.77626, -2.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[2]},
                          output="numeric")

irf2 <- counterfactual - nochange
irf2
plot(ts(irf2))

## shock on import +1% point on Import
forecasts.data <- Reduce(nextForecast, 1:20,
                         init=c(5.55556, -0.77626, -3.53407, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[3]},
                    output="numeric")

forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(5.55556, -0.77626, -2.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[3]},
                          output="numeric")

irf3 <- counterfactual - nochange
irf3
plot(ts(irf3))


#################################################

#### Using Sign Restriction to check political claim

#################################################

nleq.obj <- function(par) {
  b <- -10
  c <- -10
  d <- par[1]
  f <- -10
  g <- par[2]
  h <- par[3]
  sump <- par[4]
  sgdp <- par[5]
  simp <- par[6]
  dev1 <- sump + (b^2)*sgdp + (c^2)*simp - 7.679250e-29
  dev2 <- d*sump + b*sgdp + c*f*simp + 6.420425e-27
  dev3 <- g*sump + b*h*sgdp + c*simp - 6.709431e-31
  dev4 <- d*sump + sgdp + f^2*simp - 2.745656e-23
  dev5 <- d*sump + h*sgdp + f*simp - 7.616497e-28
  dev6 <- g^2*sump + h^2*sgdp + simp - 2.828570e-30
  return(c(dev1 , dev2 , dev3 , dev4 , dev5 , dev6))
}
nleq.obj(c(0,0,0,0,0,0))
nleqslv(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1), nleq.obj)

## shock on import +1% point on GDP
## Value of d is too small
forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(5.55556, -0.77626, -2.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")

irf2 <- counterfactual - nochange
irf2
plot(ts(irf2))




#####################################
#               GMM                 #
#####################################

##(b=d=h=0)
gmm.obj <- function(par, data) {
  ump.res <- data[, "ump.res"]
  gdp.res <- data[, "gdp.res"]
  imp.res <- data[, "imp.res"]
  b <- 0
  c <- par[1]
  d <- 0
  f <- par[2]
  g <- par[3]
  h <- 0
  sump <- par[4]
  sgdp <- par[5]
  simp <- par[6]
  dev1 <- (u.res^2) + (b^2)*(x.res^2) + (c^2)*(m.res^2) - 7.679250e-29
  dev2 <- d*(u.res^2) + b*(x.res^2) + c*f*(m.res^2) + 6.420425e-27
  dev3 <- g*(u.res^2) + b*h*(x.res^2) + c*(m.res^2) - 6.709431e-31
  dev4 <- d*(u.res^2) + (x.res^2) + f^2*(m.res^2) - 2.745656e-23
  dev5 <- d*(u.res^2) + h*(x.res^2) + f*(m.res^2) - 7.616497e-28
  dev6 <- g^2*(u.res^2) + h^2*(x.res^2) + (m.res^2) - 2.828570e-30
  return (ts(c(dev1 , dev2 , dev3 , dev4 , dev5 , dev6)))
}
gmm.obj(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1), res)
gmm(gmm.obj, res, c(0, 0.1, 0.1, 0.1, 0.1, 0.1))
summary(gmm(gmm.obj, x=res, t0 = c(0, 0.1, 0.1, 0.1, 0.1, 0.1)))

varfit <- VAR(ts.combine(ump, gdp, imp), 1)
F <- formF(varfit)
F

nextForecast <- function(lastForecast, newinfo) {
  return(F %*% lastForecast)
}

last(ump)
last(gdp)
last(imp)

# import shock on ump
forecasts.data <- Reduce(nextForecast, 1:20,
                         init=c(5.55556, -0.77626, -3.53407, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")

forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(-8.59664, -0.77626,-2.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")
counterfactual

irf4 <- counterfactual - nochange
plot(ts(irf4))


# import shock on gdp
forecasts.data <- Reduce(nextForecast, 1:20,
                         init=c(5.55556, -0.77626, -3.53407, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[2]},
                    output="numeric")

forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(5.55556, -13.36806 ,-2.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[2]},
                          output="numeric")
counterfactual

irf5 <- counterfactual - nochange
plot(ts(irf5))



# GDP shock 1% on Ump
forecasts.data <- Reduce(nextForecast, 1:20,
                         init=c(5.55556, -0.77626, -3.53407, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")

forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(5.55556, 0.22374 ,-3.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")
counterfactual

irf6 <- counterfactual - nochange
plot(ts(irf6))

# GDP shock 1% on imp
forecasts.data <- Reduce(nextForecast, 1:20,
                         init=c(5.55556, -0.77626, -3.53407, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[3]},
                    output="numeric")

forecasts.counterfactual <- Reduce(nextForecast, 1:20,
                                   init=c(5.55556, 0.22374 ,-3.53407, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[3]},
                          output="numeric")
counterfactual

irf7 <- counterfactual - nochange
plot(ts(irf7))

##############################################

#### sign restrictions

##############################################
# Acceptable: Ump response to imp shock is non-positive for h=1,...,12
irf.solve <- function(b.guess, c.guess, f.guess) {
  # Objective function depends on b.guess, c.guess, f.guess
  nleq.obj <- function(par) {
    b <- b.guess
    c <- c.guess
    d <- par[1]
    f <- f.guess
    g <- par[2]
    h <- par[3]
    sump <- par[4]
    sgdp <- par[5]
    simp <- par[6]
    dev1 <- sump + (b^2)*sgdp + (c^2)*simp - 7.679250e-29
    dev2 <- d*sump + b*sgdp + c*f*simp + 6.420425e-27
    dev3 <- g*sump + b*h*sgdp + c*simp - 6.709431e-31
    dev4 <- d*sump + sgdp + f^2*simp - 2.745656e-23
    dev5 <- d*sump + h*sgdp + f*simp - 7.616497e-28
    dev6 <- g^2*sump + h^2*sgdp + simp - 2.828570e-30
    return(c(dev1 , dev2 , dev3 , dev4 , dev5 , dev6))
  }
  # Solve for our guesses
  soln <- nleqslv(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1), nleq.obj)
  d.value <- soln$x[1]
  g.value <- soln$x[2]
  h.value <- soln$x[3]
  
  # Compute counterfactual forecasts using
  # c = c.value
  # init=c(0.167, 1.30668 , 5.43885, 1)
  forecasts.counterfactual <- 
    Reduce(nextForecast, 1:12,
           init=c(1.167, 1.30668+d.value , 5.43885, 1),
           accumulate=TRUE)
  # Collect the forecasts of inflation
  counterfactual <- 
    collect(forecasts.counterfactual,
            transform=function(el) {el[1]},
            output="numeric")
  # Calculate the IRF
  counterfactual - nochange
  
  # Is this acceptable?
  # Are all values non-positive?
  return(list(
    check=all( (counterfactual - nochange) < 0.0 ),
    b=b.guess , c=c.guess, f=f.guess))
}


irf.solve(0.1, 0.1, 0.1)

x<- expand.grid(seq(0, 10, by=0.1), seq(0, 10, by=0.1), seq(0, 10, by=0.1)) 
sign.check <- apply(x, MARGIN=2 , irf.solve)
sign.check
# Find all elements where $check is TRUE
foo <- function(element) {
  return(element$check)
}
foo(list(b=0.4, check=TRUE))
Filter(foo, sign.check)

### since this code does not work i manually replaced guessing value to check sign restriction

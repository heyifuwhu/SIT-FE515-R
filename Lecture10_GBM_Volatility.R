# ==================== 1. Variance and Realized Volatility =======================

# get data
library(quantmod)
spy <- getSymbols("SPY", auto.assign = F)
spy <- data.frame(spy)
plot.ts(spy)
spy.rtn <- diff(log(spy$SPY.Adjusted))
plot.ts(spy.rtn)

# variance
var(spy.rtn)
# [1] 0.0001862294

# daily realized variance
realized.var <- sum(spy.rtn^2)/length(spy.rtn)
realized.var

# daily realized volatility
realized.vol <- sqrt(realized.var)
realized.vol


# How to calculate yearly realized variance (Guanghua Lian, Carl Chiarella, Petko S. Kalve(2014))
# RV(N) = AF/N * sum(log(St/St-1)^2)
# AF is the anualization factor
# N is number of observation

# ==================== 2. BS model and Implied Volatility =======================
# To calculate implied volatility, we can use reversed BS model to calculate it

# BS model
CallOptionPricingBS <- function(risk.free, 
                                time.to.maturity, 
                                strike.price, 
                                spot.price, 
                                vol){
  
  d1 <- (log(spot.price/strike.price) + (risk.free + 0.5 * vol^2) * time.to.maturity) / (vol * sqrt(time.to.maturity))
  
  d2 <- d1 - vol*sqrt(time.to.maturity)
  
  call.option.price <- pnorm(d1) * spot.price - pnorm(d2) * strike.price * exp(-risk.free * time.to.maturity)
  
  return(call.option.price)
}




CallOptionPricingBS(risk.free = 0, 
                    time.to.maturity = 1/252,
                    strike.price = 220,
                    spot.price = 277.76,
                    vol = 1.29)

# Implied volatility
BSIMPVol <- function(spot.price, 
                     time.to.maturity, 
                     risk.free,
                     strike.price,
                     option.price,
                     initial.guess){
  
  fx <-  function(sig){
    d1 <- (log(spot.price/strike.price) + (risk.free + sig^2 * 0.5) * time.to.maturity) / (sig * sqrt(time.to.maturity))
    d2 <- d1 - sig * sqrt(time.to.maturity)
    value <- spot.price * pnorm(d1) - strike.price * exp( -risk.free * time.to.maturity) * pnorm(d2) - option.price
    return(value)
  }
  
  dfx <- function(sig){
    d1 <- (log(spot.price/strike.price) + (risk.free + sig^2 * 0.5) * time.to.maturity) / (sig*sqrt(time.to.maturity))
    d2 <- d1 - sig * sqrt(time.to.maturity)
    value <- strike.price * exp( -risk.free * time.to.maturity) * dnorm(d2) * sqrt(time.to.maturity)
    return(value)
  }
  
  q1 <- function(x0){
    epsilon <- 0.001  # convergent condition
    while(1){
      tmp <-  x0
      deltaX  <-  fx(x0) / dfx(x0)
      x0  <-  x0 - deltaX
      if (abs(x0-tmp) < epsilon)
      {
        break  
      }else{
        print(x0)
      }
    }
    return(x0)
  }
  
  IMPvol <- q1(initial.guess)
  return(IMPvol)
}

BSIMPVol(spot.price = 277.76,
         time.to.maturity = 1/252,
         risk.free = 0,
         strike.price = 277.5,
         option.price = 1.14,
         initial.guess = 2)
# == In Class == #

spot.price = 277.76
time.to.maturity = 1/252
risk.free = 0
strike.price = 277.5
option.price = 1.14

fx <-  function(sig){
  d1 <- (log(spot.price/strike.price) + (risk.free + sig^2 * 0.5) * time.to.maturity) / (sig * sqrt(time.to.maturity))
  d2 <- d1 - sig * sqrt(time.to.maturity)
  value <- spot.price * pnorm(d1) - strike.price * exp( -risk.free * time.to.maturity) * pnorm(d2) - option.price
  return(value)
}

x <- seq(from = 0, to = 2, by = 0.01)
y <- fx(x)
plot(x,y)

#========================== 3. GBM ==================================

rm(list = ls())

## parameters
r <- 0.05
sigma <- 0.2
Maturity <- 1
steps <- 252
S0 <- 100



## method 1: Euler Method (discretized solution)
dt <- Maturity / steps
epsilon.t.vec <- rnorm(steps)
dwt.vec <- epsilon.t.vec * sqrt(dt)
St.vec <- c()
St.vec[1] <- S0
for(i in 1:steps){
    St.vec[i+1] <- St.vec[i] + r * St.vec[i] * dt + sigma * St.vec[i] * dwt.vec[i]
}
plot(St.vec)


## method 2: Solution to GBM (continues solution)

ST <- S0 * exp((r - 0.5 * sigma^2) * Maturity + sigma * sum(dwt.vec))

St.vec[253]
ST

#Two methods will gives you similar FINAL result.






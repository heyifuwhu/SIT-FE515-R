library(quantmod)

# Q1
aapl <- getOptionChain("AAPL", Exp = "2019-03-15")
r <- 0.0225
St <- 171.86
K <- 185
T.T <- as.Date("2019-03-15")
T.t <- as.Date("2018-11-26")
t <- seq.Date(from = T.t, to = T.T, by = "day")
weekdays.Date(t)

t.t.m <- as.numeric(((T.T - T.t) + 3) / 7 * 5) / 252


AAPL <- getSymbols("AAPL", from = "2017-11-26", to = "2018-11-26", auto.assign = F)
AAPL <- data.frame(AAPL)
aapl.rtn <- diff(log(AAPL$AAPL.Adjusted))
realized.var <- sum(aapl.rtn^2)/length(aapl.rtn)
realized.vol <- sqrt(realized.var)
realized.vol

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

CallOptionPricingBS(risk.free = r, 
                    time.to.maturity = t.t.m,
                    strike.price = K,
                    spot.price = St,
                    vol = realized.vol)

# Estimation is not good
# Maybe because of the input of r

bid <- 7.30
ask <- 7.40
c.t <- (bid + ask) / 2

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
    epsilon <- 0.001 
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

sigma <- BSIMPVol(spot.price = St,
         time.to.maturity = t.t.m,
         risk.free = r,
         strike.price = K,
         option.price = c.t,
         initial.guess = 2)
# will give out error if we let initial.guess = realized.vol

# Q2

Maturity <- t.t.m
steps <- 252
S0 <- St
path <- 2000
result <- NULL

for (i in 1:path){
  dt <- Maturity / steps
  epsilon.t.vec <- rnorm(steps)
  dwt.vec <- epsilon.t.vec * sqrt(dt)
  ST <- S0 * exp((r - 0.5 * sigma^2) * Maturity + sigma * sum(dwt.vec))
  result <- c(result, ST)
}
ST.mean <- mean(result)
ST.mean

library(tseries)
jarque.bera.test(result)
# Not normal
jarque.bera.test(log(result))
# Is normal

# Q3
setwd("/Users/yigewang/Library/Mobile Documents/com~apple~CloudDocs/FE/FE515")
data <- read.csv("grape crops.csv", header=T)
yield <- data$yield
cluster <- data$cluster.count
len <- length(yield)
y <- yield
x <- cluster

fb <- function(b)
{
  f <-  0
  for (i in 1:len){
    f <-  f + 1/2 * (y[i] - x[i] * b) ^ 2
  }
  return (f)
}

df <- function(b)
{
  f <-  0
  for (i in 1:len){
    f <-  f +  (y[i] - x[i] * b) * (-x[i])
  }
  return (f)
}

plot(fb, xlim = c(0.02, 0.07), main = "f(beta)")

x0 <- 0.06
alpha <-  0.01
epsilon <-  0.0001
step <-  1

while(1)
{
  x1 <-  x0 - alpha * df(x0)
  if (abs(fb(x1) - fb(x0)) < epsilon)
  {
    cat("x = ", x1, '\n', sep='')
    cat("Final step: ", step, '\n', sep='')
    break
  }
  x0 <-  x1
  step <-  step + 1
}

# not running anw

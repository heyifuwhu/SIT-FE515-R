# I pledge my honor that I have abided by the Stevens Honor System.

# ====================================================================================== #
# ====== Question 1 ===== #
# -------------------------------------------------------------------------------------- #

library(quantmod)
library(timeDate)
library(tseries)
library(fUnitRoots)

# define function "analyze"
analyze <- function(ticker, start.date, end.date){
  
  # get data from yahoo finance
  data <- getSymbols(ticker, src = "yahoo", from = start.date, to = end.date, auto.assign = F )
  
  # get price and calculate return
  price <- data[,6]
  rtn <- diff(price, lag = 1) / price[-length(price)]
  rtn <- rtn[-1,]
  
  # calculate skewness
  skew <- skewness(rtn)
  
  # calculate kurtosis
  kurt <- kurtosis(rtn)

  # apply adf test to test stationary
  stat <- adfTest(rtn)
  stat.p <- stat@test$p.value
  stat.result <- F
  if (stat.p <= 0.05){
    stat.result <- T
  }
  
  # apply jarque bera test to test normality
  norm <- jarque.bera.test(rtn)
  norm.p <- norm$p.value
  norm.result <-T
  if (norm.p <= 0.05){
    norm.result <- F
  }
  
  # create dataframe
  df <- data.frame(Ticker = ticker, Skewness = skew, Kurtosis = kurt, 
                   Stationary = stat.result, Normality = norm.result)
  return(df)
}



# assign 10 equities and start/end date
tickers <- c("AAPL","BAC","C","DIS","F","GS","JPM","MS","PG","TWTR")
sd <- as.Date("2017/01/01")
ed <- as.Date("2017/12/31")

# get result with mapply and manipulate
result1 <- as.data.frame(mapply(analyze, tickers, sd, ed))
result1 <- result1[-1,]

result2 <- t(result1)
result2 <- as.data.frame(result2)
result2 <- as.data.frame(lapply(result2, unlist))

# sort the kurtosis
result2[order(result2$Kurtosis),]

# In this case, C has the lowest kurtosis
# However, it is heavy tail because here kurtosis is excess kurtosis
# having positive excess means heavy tail



# ====================================================================================== #
# ====== Question 2 ===== #
# -------------------------------------------------------------------------------------- #
library(lubridate)

# get data from csv file
setwd("/Users/yigewang/Library/Mobile Documents/com~apple~CloudDocs/FE/FE515")
BA <- read.csv("BA.csv")

# take subset of "Trade" and assign strptime
BA.trade <- subset(BA, BA$Type == "Trade")
BA.trade$Time <- strptime(BA.trade$RTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York") 

# split the data with 5mins time window
t1 <-  BA.trade$Time[1]
t2 <-  BA.trade$Time[1]
t2$min <-  t2$min + 5 * 78

split.data <- split(BA.trade, cut(BA.trade$Time,
                    seq.POSIXt(from = t1, 
                               to = t2, by = "5 min")))

# calculate returns and save them in vector
rtns <- c()
for (i in 1:78){
  data <- split.data[i]
  data <- as.data.frame(data)
  len <- length(data[,9])
  ps <- data[1,9]
  pe <- data[len,9]
  rtn <- (pe - ps) / ps
  print(rtn)
  rtns[i] <- rtn
}

# test stationary
adfTest(rtns)

# P-value here is 0.01, which is significant
# We reject our null hypothesis and take alternative hypothesis, 
# which indicate this dataset is stationary

# test normality
jarque.bera.test(rtns)

# P-value here is significant, we will reject our null hypothesis
# this dataset is not normally distributed




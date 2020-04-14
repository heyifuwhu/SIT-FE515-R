# I pledge my honor that I have abided by the Stevens Honor System.

# Question 1

# 1.1
dgeom(5, 1/6)

# 1.2
dice <- c(1,2,3,4,5,6)
count <- 0
for (i in 1:1000){
  observation <- sample(dice, 3, replace = T)
  if (observation[1] == observation[2] || observation[2] == observation[3]){
    count <- count + 1
  }
}
prob <- count / 1000
prob


# Question 2
library(quantmod)
library(tseries)
equities <- c("AAPL","BAC","C","DIS","F","GS","JPM","MS","PG","TWTR","WFC","XLF")
returns <- c()
for (equity in equities){
  data <- getSymbols(equity, src = "yahoo", from = "2015-01-01", to = "2017-12-31", auto.assign = F )
  stock.rtn <- dailyReturn(data)
  stock.df <- as.data.frame(data)
  
  jarque.bera.test(stock.rtn)
  # ????????????
  
  returns <- cbind(returns, stock.rtn)
}

jarque.bera.test(returns[,1])
jarque.bera.test(returns[,2])
jarque.bera.test(returns[,3])
jarque.bera.test(returns[,4])
jarque.bera.test(returns[,5])
jarque.bera.test(returns[,6])
jarque.bera.test(returns[,7])
jarque.bera.test(returns[,8])
jarque.bera.test(returns[,9])
jarque.bera.test(returns[,10])
jarque.bera.test(returns[,11])
jarque.bera.test(returns[,12])

# All P-values here are significant, we will reject our null hypothesis
# They are all not normal distribution

returns <- as.data.frame(returns)
date <- as.Date(row.names(returns))
returns <- cbind(date, returns)
returns1 <- subset(returns, date < "2017/01/01")

result <- lm(daily.returns.11 ~ daily.returns + 
               daily.returns.1 +
               daily.returns.2 +
               daily.returns.3 +
               daily.returns.4 +
               daily.returns.5 +
               daily.returns.6 +
               daily.returns.7 +
               daily.returns.8 +
               daily.returns.9 +
               daily.returns.10, data = returns1)
summary(result)

getSymbols("XLF", src = "yahoo", from = "2015-01-01", to = "2017-12-31")
XLF <- as.data.frame(XLF)
Date <- as.Date(row.names(XLF))
plot(Date, XLF$XLF.Close, 
     type = "l", 
     main = "Stock Price Trend of XLF", 
     xaxt = "n",
     yaxt = "n",
     xlab = "Date",
     ylab = "Stock Price")
axis.Date(side=1, at=cut(Date, "months"), format="%Y/%m/%d")
axis(side = 2, at = seq(from = 15, to = 30, by = 2))
lines(Date, XLF$XLF.High, type = "l", col = "red", lwd = 1)
lines(Date, XLF$XLF.Low, type = "l", col = "green", lwd = 1)

x <- c(Date, rev(Date))
y <- c(XLF$XLF.High, rev(XLF$XLF.Low))
polygon(x,y, col = "grey", border = NA)
legend("topleft", 
       c("High value", "Low value", "Difference in between", 
         "Close value"), 
       fill = c("red", "green", "grey", "black"),
       title = "Legend")


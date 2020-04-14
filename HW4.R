# I pledge my honor that I have abided by the Stevens Honor System.

# ====================================================================================== #
# ====== Question 1 ===== #
# -------------------------------------------------------------------------------------- #

library(quantmod)
library(tseries)

getSymbols("SPY", source = "yahoo", from = "2017-01-01", to = "2017-12-31")
data <- SPY$SPY.Close

data1 <- data[6:length(data)]
ema5 <- mean(data[1:5])
alpha1 <- 2 / (5 + 1)
for (i in 1:length(data1)){
  ema <- (data1[i]-ema5[i])*alpha1 + ema5[i]
  ema5 <- c(ema5, ema)
}
# Got the same answer as the EMA function in R
# EMA(data, n = 5)


data2 <- data[13:length(data)]
data3 <- data[27:length(data)]
ema12 <- mean(data[1:12])
ema26 <- mean(data[1:26])
alpha2 <- 2 / (12 + 1)
alpha3 <- 2 / (26 + 1)
for (i in 1:length(data2))
{
  ema <- (data2[i]-ema12[i])*alpha2 + ema12[i]
  ema12 <- c(ema12, ema)
}
for (i in 1:length(data3))
{
  ema <- (data3[i]-ema26[i])*alpha3 + ema26[i]
  ema26 <- c(ema26, ema)
}
ema12 <- ema12[15:length(ema12)]
macd <- ema12 - ema26

# Still got same answer for ema12 and ema28
# EMA(data, n = 12)
# EMA(data, n = 26)
# But different for macd
# MACD(data)

data <- data[26:length(data),]
df <- as.data.frame(data)
date <- as.Date(row.names(df))

par(mfrow = c(2,1))
plot(date, data, 
     type = "l", 
     main = "SPY Price Movements", 
     xaxt = "n",
     yaxt = "n",
     xlab = "Date",
     ylab = "Stock Price")
axis.Date(side=1, at=cut(date, "months"), format="%Y/%m/%d")
axis(side = 2, at = seq(from = 210, to = 290, by = 10))
plot(date, macd,
     type = "l", 
     main = "MACD Movements", 
     xaxt = "n",
     yaxt = "n",
     xlab = "Date",
     ylab = "MACD")
axis.Date(side=1, at=cut(date, "months"), format="%Y/%m/%d")
axis(side = 2, at = seq(from = 0, to = 3, by = 0.5))

# ====================================================================================== #
# ====== Question 2 ===== #
# -------------------------------------------------------------------------------------- #

# 2-1.1
par(mfrow=c(1,1))
setwd("/Users/yigewang/Library/Mobile Documents/com~apple~CloudDocs/FE/FE515")
library(readxl)
df <- read_xlsx("data for Q2 and Q3.xlsx")
percent <- df$PercBorrow
debt <- df$AvgDebt
plot(percent, debt)
lm(debt~percent)
summary(lm(debt~percent))
lm.r <- lm(debt~percent)
abline(lm.r, col='red')

# 2-1.2
# If you are asking the change of average debt with 10% more students borrowing
change.of.debt <- 10 * lm.r$coefficients[2] # 1689.845

# If you are asking the average debt when the percentage of borrowing is 10%
debt.at.10 <-10 * lm.r$coefficients[2] + lm.r$coefficients[1] # 13507.73

# If you are sking the average debt when the percentage of borrowing is 90%
debt.at.90 <-90 * lm.r$coefficients[2] + lm.r$coefficients[1] # 27026.48


# 2.2
admit <- df$Admit
Yr4 <- df$Yr4Grad
iaa <- df$InAfterAid
oaa <- df$OutAfterAid
aa <- df$AvgAid
spf <- df$StudPerFac

factors <- cbind(percent, admit, Yr4, iaa, oaa, aa, spf)
par(mfrow=c(3,3))
result <- NULL

for (i in 1:ncol(factors)){
  plot(factors[,i], debt, main = colnames(factors)[i], xlab = colnames(factors)[i])
  lm.r <- lm(debt ~ factors[,i])
  abline(lm.r, col='red')
  temp <- summary(lm.r)
  r.squared <- temp$r.squared
  result <- rbind(result, r.squared)
}

rn <- c("percent", "admit", "Yr4", "IAA", "OAA", "AA", "SPF")
row.names(result) <- rn
colnames(result) <- "R Squared"
result <- as.data.frame(result[order(result[,1], decreasing = TRUE),1])
colnames(result) <- "R Squared"
result

# After building linear regression model for each of the variables
# and sorting them according to their corresponding R squared value
# We can see that not all models look linear, e.g. Yr4Grad.
# The best single predictor should be InAfterAid, with the highest 
# R squared value.

# ====================================================================================== #
# ====== Question 3 ===== #
# -------------------------------------------------------------------------------------- #

model1 <- lm(AvgDebt ~ Admit + Yr4Grad + StudPerFac + InAfterAid + OutAfterAid + AvgAid + PercBorrow, data = df)
summary(model1)
# One non-significant variable StudPerFac, p-value < 0.05

null.model <- lm(AvgDebt ~ 1, data = df)
full.model.formula <- AvgDebt ~ Admit + Yr4Grad + StudPerFac + InAfterAid + OutAfterAid + AvgAid + PercBorrow
step(null.model, full.model.formula, direction = "both")
# Have non-significant variables
# Only InAfterAid and AvgAid are significant

# It is hard to say that which of the two models are better
# bascially because they have different criteria 
# If I have to choose one, I would say the second model is better.
# Because it can help you efficiently get rid of non-significant variables
# Though the first model can help you with that as well, the restriction 
# is not strict.



# ====================================================================================== #
# ====== Question 4 ===== #
# -------------------------------------------------------------------------------------- #

library(timeDate)

tickers <- c("AAPL","BAC","C","DIS","F","GS","JPM","MS","PG","TWTR")
start.date <- as.Date("2015/01/01")
end.date <- as.Date("2017/12/31")

result <- NULL

for (ticker in tickers){
  data <- getSymbols(ticker, src = "yahoo", from = start.date, to = end.date, auto.assign = F )
  rtn <- monthlyReturn(data)
  result <- cbind(result, rtn)
}
colnames(result) <- tickers

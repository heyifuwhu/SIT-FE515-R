# ======================== Advanced quantmod========================
source("getOptionChain_Ziwen.R")

option.list <- getOptionChainZiwen("MSFT")

call.option.table <- option.list$calls

head(call.option.table)

# Strike                 LTD  Last        Chg   Bid   Ask Vol  OI
# MSFT181012C00085000   85.0 2018-10-05 23:48:13 27.33  0.0000000 25.45 29.25   2   0
# MSFT181012C00090000   90.0 2018-10-05 23:48:14 22.16  0.0000000 20.60 24.25  17  14
# MSFT181012C00090500   90.5 2018-10-05 23:48:14 22.10  0.0000000 19.95 23.75   1   1
# MSFT181012C00095000   95.0 2018-10-05 15:53:24 17.55 -0.6800003 15.45 19.25  13  10
# MSFT181012C00099000   99.0 2018-09-10 09:47:39 10.17  0.0000000 13.80 15.25   5 121
# MSFT181012C00099500   99.5 2018-09-06 15:59:59 14.45  0.0000000 14.10 14.35  10  76

library(quantmod)

option.list <- getOptionChain("MSFT")

call.option.table <- option.list$calls

head(call.option.table)

# Strike  Last        Chg   Bid   Ask Vol  OI
# MSFT181012C00085000   85.0 27.33  0.0000000 25.45 29.25   2   0
# MSFT181012C00090000   90.0 22.16  0.0000000 20.60 24.25  17  14
# MSFT181012C00090500   90.5 22.10  0.0000000 19.95 23.75   1   1
# MSFT181012C00095000   95.0 17.55 -0.6800003 15.45 19.25  13  10
# MSFT181012C00099000   99.0 10.17  0.0000000 13.80 15.25   5 121
# MSFT181012C00099500   99.5 14.45  0.0000000 14.10 14.35  10  76


# ======================== Return ===================================

## simple return
# R_t = (P_t - P_t-1) / P_t-1
rm(list = ls())

library(quantmod)
msft <- getSymbols("MSFT", auto.assign = F)
msft <- data.frame(msft)
head(msft)

msft.price <- msft$MSFT.Adjusted
msft.pt <- msft.price[2:length(msft.price)]
msft.pt1 <- msft.price[1: (length(msft.price)-1) ]

msft.simple.return <- (msft.pt - msft.pt1) / msft.pt1
msft.log.return <- log(msft.pt) - log(msft.pt1)

par(mfrow=c(2, 1))    	# combine two plots together 
plot.ts(msft.simple.return, ylab = 'simple return')
plot.ts(msft.log.return, ylab = 'log return')

#From plot you should able to see simple return and log return looks same
msft.simple.return - msft.log.return
mean( abs(msft.simple.return - msft.log.return))


#===================== In class exercise ========================
# use for loop to download two equities (AAPL and MSFT) with 1 year length of data
# and do following things:
# 1. calculate log return using definition
# 2. calculate simple return using periodReturn()


AAPL <- getSymbols("AAPL", auto.assign = F)
AAPL <- data.frame(AAPL)


MSFT <- getSymbols("MSFT", auto.assign = F)
MSFT <- data.frame(MSFT)





#=================================Skewness and Kurtosis============================
library(timeDate)
x <- rnorm(100000)

skewness(x)
kurtosis(x)

skewness(x)
# [1] -0.0008174494
# attr(,"method")
# [1] "moment"
kurtosis(x)
# [1] 0.002277514
# attr(,"method")
# [1] "excess"

skewness(msft.simple.return)
kurtosis(msft.simple.return)
# > skewness(msft.simple.return)
# [1] 0.4824976
# attr(,"method")
# [1] "moment"
# > kurtosis(msft.simple.return)
# [1] 10.89886
# attr(,"method")
# [1] "excess"

skewness(msft.log.return)
kurtosis(msft.log.return)
# > skewness(msft.log.return)
# [1] 0.173612
# attr(,"method")
# [1] "moment"
# > kurtosis(msft.log.return)
# [1] 9.761736
# attr(,"method")
# [1] "excess"

#===========================In class exercise==============================
# Calculate the log return of JPM and answer following questions
# 1. is the data right skewed or left skewed
# 2. is the data has heavy tail or short tail


JPM <- getSymbols("JPM", auto.assign = F)
JPM <- data.frame(JPM)
head(JPM)

JPM.price <- JPM$JPM.Adjusted
JPM.pt <- JPM.price[2:length(JPM.price)]
JPM.pt1 <- JPM.price[1: (length(JPM.price)-1) ]

JPM.log.return <- log(JPM.pt) - log(JPM.pt1)

skewness(JPM.log.return)
kurtosis(JPM.log.return)

#=============================Normality Test===============================================
library(tseries)
# we will use jarque bera test to test normality

jarque.bera.test(x)
# Null: It is normal (The skewness being zero and the excess kurtosis being zero)
# Alternative: At least one condition is not satisfied


# Jarque Bera Test
# 
# data:  x
# X-squared = 2.5393, df = 2, p-value = 0.2809

#From above we can see the P-value is not significant
#We fail to reject our null hypothesis

jarque.bera.test(msft.simple.return)
# > jarque.bera.test(msft.simple.return)
# 
# Jarque Bera Test
# 
# data:  msft.simple.return
# X-squared = 14795, df = 2, p-value < 2.2e-16

#P-value here is significant, we will reject our null hypothesis

jarque.bera.test(msft.log.return)

# > jarque.bera.test(msft.log.return)
# 
# Jarque Bera Test
# 
# data:  msft.log.return
# X-squared = 11792, df = 2, p-value < 2.2e-16

#P-value here is significant, we will reject our null hypothesis


#==========================Stationary Test=================================
library(fUnitRoots)
#Stationary test
# Null hypothesis: it is not stationary (At least one unit root is present in a time series sample.)
# Alternative hypothesis: it is stationary (There is no unit root.)

adfTest(msft.simple.return)
# Title:
#   Augmented Dickey-Fuller Test
# 
# Test Results:
#   PARAMETER:
#   Lag Order: 1
# STATISTIC:
#   Dickey-Fuller: -42.2269
# P VALUE:
#   0.01 
# 
# Description:
#   Sun Oct 07 19:45:16 2018 by user: Yarch


#P-value here is 0.01, which is significant
#We reject our null hypothesis and take alternative hypothesis, 
#which indicate this dataset is stationary

adfTest(msft.log.return)
# Title:
#   Augmented Dickey-Fuller Test
# 
# Test Results:
#   PARAMETER:
#   Lag Order: 1
# STATISTIC:
#   Dickey-Fuller: -42.0832
# P VALUE:
#   0.01 
# 
# Description:
#   Sun Oct 07 19:46:05 2018 by user: Yarch

#P-value here is 0.01, which is significant
#We reject our null hypothesis and take alternative hypothesis, 
#which indicate this dataset is stationary





#===========================ACF plot===========================================================
#ACF plot is a method used to determin the order of AR model
#Here we do not use this property, we only use this to obtain auto-correlation value

acf(msft.simple.return)
auto.correlation <- acf(msft.simple.return)$acf

#Return me auto-correlation from lag 0 to lag 10
auto.correlation[1:11]

#check whether it is correct
test.auto.correlation <- vector()
n <- length(msft.simple.return)
for(i in 1:10){
  test.auto.correlation[i] <- cor(msft.simple.return[1:(n-i)],msft.simple.return[(1+i):n])
}




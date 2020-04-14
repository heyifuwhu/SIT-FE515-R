library(quantmod)
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


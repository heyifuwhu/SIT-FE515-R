# Agenda: 
# 1. Package quantmod
# 2. Intro to Bloomberg API
# 3. Intro Thomson Reuters Tick History
# 4. TRTH R API


# =============== quantmod ================

rm(list = ls())

library(quantmod)
# quantmod package document:
# 
getSymbols(Symbols = c("MSFT","AAPL","BAC","GS"), src = "yahoo")

MSFT$MSFT.open #not working because it is an xts object

MSFT <- data.frame(MSFT)
head(MSFT)

# set time interval
getSymbols(Symbols = "INTC", from = "2001-01-01", to = "2001-12-31")
INTC <- data.frame(INTC)
head(INTC)    
tail(INTC)


# assign data to a custom name
getSymbols("AAPL", auto.assign = F)
aapl <- getSymbols("AAPL", auto.assign = F)


# by default 
# src = "yahoo"
# from = "2007-01-01"
# to = today
# auto.assign = T


chartSeries(MSFT)
chartSeries(MSFT,theme=chartTheme('white'))
chartSeries(MSFT,TA=NULL)   #no volume
chartSeries(MSFT,TA=c(addBBands()))  #add volume and Bollinger Bands from TTR
chartSeries(MSFT,TA=c(addVo()))

getDividends("AAPL", auto.assign = F)

#OptionChain will be saved into a list
#For option chain, auto.assign is not needed
aapl <- getOptionChain("AAPL")

aapl.call.option <- aapl$calls

aapl.put.option <- aapl$puts

#EXP requires an ISO-8601 style string
aapl <- getOptionChain("AAPL", Exp = "2018-03-02")

#Fetch current stock quote(s) 
getQuote("AAPL")


#Additional feature when using quantmod
#Calculate return
periodReturn(x,
             period='monthly',
             subset=NULL,
             type='arithmetic',
             leading=TRUE,
             ...)

#type: type of returns: arithmetic (simple return) or log (log return)
#x: input dataset, object of state prices, or an OHLC type object

#If you download the data using quantmod, you can use this function
#Otherwise it will not work

#Good example
getSymbols("JPM")
dailyReturn(JPM)

bac <- getSymbols("BAC", auto.assign = F)
monthlyReturn(bac)


#Bad example

jpm <- data.frame(JPM)
dailyReturn(jpm)


# ======================== Bloomberg API=============================
install.packages("Rblpapi")
library(Rblpapi)


con <- blpConnect(host=getOption("blpHost", "127.0.0.1"),
           port=getOption("blpPort",8194L))


#Bloomberg Data History
bdh("SPY US Equity", c("PX_LAST", "VOLUME"), 
    start.date=Sys.Date()-31)

#Bloomberg Data Point
#You can get current price for corresponding input
bdp(c("ESA Index", "SPY US Equity"), c("PX_LAST", "VOLUME"))

#Bloomberg Data set
#You need this function to download option chain
bds("IBM US Equity", field = c("OPT_CHAIN"))



spy <- getBars("SPY US Equity", 
               eventType='TRADE',
               barInterval=60, #A integer denoting the number of minutes for each bar
               startTime=Sys.time() - 60*60*6,
               endTime=Sys.time(),
               options=NULL, 
               verbose=FALSE, 
               returnAs=getOption("blpType","matrix"),
               tz=Sys.getenv("TZ",unset="EST"),
               #eastern standard time
               con=defaultConnection())

getBars("ES1 Index")

getMultipleTicks("ES1 Index")


blpDisconnect(con)



# ======================== Bloomberg API Rbbg=============================

#Rbbg is a package provided by Bloomberg
#However, this package is no longer updated 
#You can use this pacakge only when other bloomberg package is not functional

# Prerequisite for Rbbg:
#     
# -latest Java
# -R version consistent with Java version (32 bit or 64 bit)
# -latest R package rJava
# -package Rbbg
# -R version has to be 3.3.3
# -Bloomberg APIv3


# 1.connecting with bloomberg
install.packages("Rbbg", repos = "http://r.findata.org") 
install.packages("rJava")
library (Rbbg)
conn_bbg <- blpConnect()
Sys.setenv(TZ="EST")

# 1.1 Simple Query examples:
securities <- c("GOOG US Equity",
                "VIX US 07/22/15 C14 Index",
                "SPX US 07/17/15 C2120 Index",
                "HSI 07/30/15 C26600 Index",
                "NDX US 07/17/15 C4520 Index",
                "CAC 07/17/15 C4850 Index") 
field <- c("PX_LAST") 
startdate <- "20150610" 
enddate <- "20150617" 

# bdh function form Rbbg packages can get present price
bdp(conn_bbg,securities,field) 

# bdh function form Rbbg packages can get historical price
bdh(conn_bbg,securities,field, startdate, enddate)

# 1.2 Bulk Data
# 1.2.2 Dividend
security <- c("BKIR ID Equity")
field <- c("DVD_HIST")
bds(conn_bbg, security, field)

# 1.2.3 getting a list of members in an index
security <- "UKX Index"
field <- "INDX_MEMBERS"
bds(conn_bbg, security, field)

# 1.3 Option chain
securities <- c("IBM US Equity")
options <- bds(conn_bbg,securities,c("OPT_CHAIN"))
n <- nrow(options);
opt <- NULL
for(i in 1:50){
  opt<-rbind(opt,
             as.data.frame(bdp(conn_bbg,c(options[i,1]),c("PX_LAST","OPT_PUT_CALL")))) 
}

option <- options[410,1]
bdp(conn_bbg,option,c("PX_LAST","OPT_PUT_CALL","PX_VOLUME"))
startdate <- "20160701"
enddate <- "20160925"

bdh(conn_bbg,option,field, startdate, enddate)

# 1.4 disconnect 
blpDisconnect(conn_bbg)

# # sample 1
# conn <- blpConnect()
# bdp(conn, "AMZN US Equity", "NAME")
# 
# # sample 2
# securities <- c("AMZN US Equity", "OCN US Equity")
# fields <- c("NAME", "PX_LAST", "TIME", "SETTLE_DT", "HAS_CONVERTIBLES") # Demo different return data types.
# bdp(conn, securities, fields)



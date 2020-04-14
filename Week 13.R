# Author: Ziwen Ye
# Usage: ANOVA analysis for paper 2


#Step 6. ANOVA result

mytable <- read.csv("anova_table.csv")

anova.model <- lm(RMSE.values ~ model.label * vol.label * maturity * ticker, data = mytable)
anova(anova.model)


#######################################################################
#### How to prepare an ANOVA table#####################################
#######################################################################

setwd("F:/New paper 2/RMSE report")

file.names <- list.files(pattern = ".csv")

final.table <- NULL

for(i in 1:length(file.names)){
  RMSE.table <- read.csv(file.names[i], stringsAsFactors = F)
  
  #Keep the data between March and May
  RMSE.table$maturity <- as.Date(RMSE.table$maturity)
  
  RMSE.table <- subset(RMSE.table, maturity < as.Date("2018-06-01"))
  
  #Keep +/- 7% data based on the open price
  open.price <- RMSE.table$open.price
  lower.bound <- 0.93*open.price
  upper.bound <- 1.07*open.price
  strike.price <- RMSE.table$strike.price
  
  target.row <- strike.price >= lower.bound & strike.price <= upper.bound
  
  RMSE.table <- RMSE.table[target.row,]
  
  #Keep useful content from the table
  
  final.RMSE.table <- RMSE.table[,c(2:4, 7, 12:15)]
  num.of.label <- nrow(RMSE.table)
  
  #Step 1. stack all values into a single vector
  
  RMSE.values <- c(final.RMSE.table$RMSE.of.AWR2M, final.RMSE.table$RMSE.of.BSAWR2M,
                   final.RMSE.table$RMSE.of.LPR2M, final.RMSE.table$RMSE.of.BSLPR2M)
  
  #Step 2. create label for models
  model.label <- c(rep("RBS_AWR", times = num.of.label), rep("BS_AWR", times = num.of.label),
                   rep("RBS_R", times = num.of.label), rep("BS_R", times = num.of.label))
  
  #step 3. create label for maturity
  maturity <- as.character(rep(final.RMSE.table$maturity, 4))
  maturity <- as.factor(as.character(maturity))
  
  #step 4. create labels for week indicator
  maturity2 <- as.character(rep(final.RMSE.table$maturity, 4))
  
  label1 <- (maturity2 == "2018-03-02"|maturity2 == "2018-04-06"|maturity2 == "2018-05-04")
  label2 <- (maturity2 == "2018-03-09"|maturity2 == "2018-04-13"|maturity2 == "2018-05-11")
  label3 <- (maturity2 == "2018-03-16"|maturity2 == "2018-04-20"|maturity2 == "2018-05-18")
  label4 <- (maturity2 == "2018-03-23"|maturity2 == "2018-04-27"|maturity2 == "2018-05-25")
  
  maturity2[label1] <- 1
  maturity2[label2] <- 2
  maturity2[label3] <- 3
  maturity2[label4] <- 4
  maturity2[maturity2 == "2018-03-29"] <- 5
  
  maturity2 <- as.factor(maturity2)
  
  #step 5. create label for ticker
  ticker <- rep(RMSE.table$ticker, 4)
  ticker <- as.factor(ticker)
  
  #step 6. create label for vol
  vol.lable <- c(rep("AWR", times = num.of.label*2), rep("R", times = num.of.label*2))
  
  #step 7. create label for BS model type
  bs.lable <- rep(c(rep("RBS", times = num.of.label), rep("BS", times = num.of.label)),2)
  
  #Step 8. prepare anova table
  mytable <- data.frame(RMSE.values, model.label, maturity, maturity2, ticker, vol.lable, bs.lable)
  
  final.table <- rbind(final.table, mytable)
}

#final.table2 <- final.table[final.table$maturity2 !=3,]

#final.table1 <- subset(final.table, maturity2==5)

anova.model <- lm(RMSE.values ~ vol.lable * bs.lable * maturity * ticker, data = final.table)
anova(anova.model)


#########################
## T-test part#####
##################
setwd("F:/New paper 2/RMSE report")

ResultEvaluation.vol <- function(ticker, dates){
  temp.file.name <- paste0(ticker, ".csv")
  rmse.result <- read.csv(temp.file.name, stringsAsFactors = F)
  
  option.types <- c("Call", "Put")
  methods <- c("BS_R", "BS_AWR")
  
  result.table <- NULL
  
  for(i in 1:2){
    sub.table <- subset(rmse.result, option.type == option.types[i])
    
    date.result <- NULL
    
    for(j in 1:length(dates)){
      temp.sub.table <- subset(sub.table, current.date == dates[j])
      
      #target colums
      AWR.rmse <- na.omit(temp.sub.table$RMSE.of.BSAWR2M)
      BS.rmse <- na.omit(temp.sub.table$RMSE.of.BSLPR2M)
      
      AWR.mean <- mean(AWR.rmse)
      BS.mean <- mean(BS.rmse)
      
      #which methods has the lowest rmse
      if(is.na(AWR.mean) & is.na(BS.mean)){
        min.method <- NA
      }else{
        min.method <- methods[which.min(c(BS.mean, AWR.mean))]
      }
      
      t.test1 <- t.test(AWR.rmse, BS.rmse)$p.value
      
      if(t.test1 > 0.05){
        t.result <- c("Same")
      }else{
        t.result <- min.method
      }
      
      temp.result <- c(ticker, as.character(dates[j]), option.types[i], t.result, min.method)
      date.result <- rbind(date.result, temp.result)
    }
    result.table <- rbind(result.table, date.result)
  }
  
  colnames(result.table) <- c("Ticker", "Date", "Option type", "T-test result", "best method")
  return(result.table)
}



for(m in 1:length(ticker.list)){
  t.test.result <- ResultEvaluation.vol(ticker.list[m], date.list)
  
  path1 <- c("F:/New paper 2/t test report/")
  path2 <- paste0(ticker.list[m], ".csv")
  final.path <- paste0(path1, path2)
  
  write.csv(t.test.result, final.path)
}
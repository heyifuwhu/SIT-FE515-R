setwd("/Users/yigewang/Library/Mobile Documents/com~apple~CloudDocs/FE/FE515")
VIX <- read.csv("VIX_2010-05-06.csv")
data <- VIX$Last
data <- na.omit(data)
m <- mean(data)
v <- var(data)
std <- sqrt(v)
d.plus <- m + 2 * std
d.minus <- m - 2 * std
result <- subset(VIX, last < d.minus | last > d.plus)

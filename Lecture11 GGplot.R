#=======================Introduction================================
library(ggplot2)
library(gcookbook) #dataset
library(gridExtra)


#========comparison between plot and qpolt==========================
#attach(mtcars)

plot(mtcars$wt,mtcars$mpg)
qplot(mtcars$wt,mtcars$mpg)
#From plots we can see qplot looks better than plot

#=======comparison between qplot and ggplot=========================


a <- qplot(mtcars$wt,mtcars$mpg)
b <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

grid.arrange(a,b)

#==================plot for function================================
myfun <- function(x){
  y <- x^2+x
  return(y)
}

x <- c(1:20)

plot(x,myfun(x))

ggplot(data.frame(x=c(0,20)),aes(x = x)) + stat_function(fun = myfun, geom = "point")

#========================line chart ===================================
timeline <-  seq(from=as.POSIXct("2010-05-06 9:30"), 
                 to=as.POSIXct("2010-05-06 15:59"),
                 by=60)

r <- 0.05
sigma <- 0.2
Maturity <- 1
steps <- 389
S0 <- 100


dt <- Maturity / steps
epsilon.t.vec <- rnorm(steps)
dwt.vec <- epsilon.t.vec * sqrt(dt)
St.vec <- c()
St.vec[1] <- S0
for(i in 1:steps){
  St.vec[i+1] <- St.vec[i] + r * St.vec[i] * dt + sigma * St.vec[i] * dwt.vec[i]
}

priceline1 <- St.vec
priceline2 <- priceline1 + 5

#When combining the data, you CAN NOT use cbind() in ggplot()
table1 <- data.frame(timeline, priceline1)

#Method 1
ggplot(table1, aes(x = timeline, y = priceline1)) + geom_line()


#Method 2
#Wrong example
ggplot() + geom_line(table1, aes(x = timeline, y = priceline1))

#Correct example
ggplot() + geom_line(data = table1, aes(x = timeline, y = priceline1), col = "red")

###Add another line to ggplot

p <- ggplot() + geom_line(data = table1, aes(x = timeline, y = priceline1), col = "red")

table2 <- data.frame(timeline, priceline2)

p + geom_line(data = table2, aes(x = timeline, y = priceline2), col = "black")


####Add legend

ggplot(table1, aes(x = timeline, y = priceline1, col = "red")) + geom_line()

# As long as you input the col inside aes(), you will get the legend automatically



######################In class exercise##################################
# Add points to the ggplot
ggplot(table1, aes(x = timeline, y = priceline1)) + geom_point(col = "black") + geom_line(col = "red")

#====================stacked bar chart==============================
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))
+ geom_bar(stat = "identity")
# wrong way to add feature into ggplot


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat = "identity", colour="black") + # add black outline
  guides(fill=guide_legend(reverse=TRUE)) +  #reverse legend content
  scale_fill_brewer(palette = "Pastel1")  #change bar color

#===================stacked area graph==============================
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=0.2, alpha=0.4) + #size control thickness of lines between different area
  #alpha control areas semitransparent level
  scale_fill_brewer(palette = "Blues", breaks=rev(levels(uspopage$AgeGroup)))
# breaks is another way to reset legend order, instead of reverse everything, here we change the order
# of content

#===================color by groups=================================
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >=0

ggplot(csub ,aes(x=Year, y=Anomaly10y, fill=pos))+
  geom_bar(stat = "identity", position = "identity")+
  scale_fill_manual(values = c("red","blue"), guide=F)
# Notice, stat is the place to determine different color, not position


#===================making correlation matrix plot==================
mcor <- cor(mtcars)
library(corrplot)
corrplot(mcor)  

corrplot(mcor, 
         method = "shade",  #using shade square instead of circle
         shade.col = NA,    #removing shades
         tl.col = "black",  #change description letter color
         tl.srt = 45)       #change word angle


lightercol <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, 
         method = "shade",  #using shade square instead of circle
         shade.col = NA,    #removing shades
         tl.col = "black",  #change description letter color
         tl.srt = 45,       #change word angle
         col = lightercol(200), 
         addCoef.col = "black", #add correlation coefficients
         order = "AOE")  #angular order of eigenvectors

#############################In class exercise#########################
#Download 5 equities and calculate log return
#In the end, generate a correlation heat plot

library(quantmod)
tickers <- c("AAPL","BAC","C","DIS","F")
sd <- as.Date("2017/01/01")
ed <- as.Date("2017/12/31")

result <- NULL
for (ticker in tickers){
  data <- getSymbols(ticker, src = "yahoo", from = sd, to = ed, auto.assign = F)
  price <- data[,6]
  rtn <- diff(log(price), lag = 1)
  rtn <- rtn[-1,]
  result <- cbind(result, rtn)
}

rcor <- cor(result)
corrplot(rcor, 
         method = "shade",  #using shade square instead of circle
         shade.col = NA,    #removing shades
         tl.col = "black",  #change description letter color
         tl.srt = 45)       #change word angle




#===========================3D plot================================
library(rgl)
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type = "s", size = 0.75, lit=F, col = c("red","green","blue"))

interleave <- function(v1,v2){
  as.vector(rbind(v1,v2))
}

segments3d(interleave(mtcars$wt, mtcars$wt),
           interleave(mtcars$disp, mtcars$disp),
           interleave(mtcars$mpg, min(mtcars$mpg)),
           alpha=0.4, col="blue")
# you are connecting two points

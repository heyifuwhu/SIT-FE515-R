
#======================== Date class in R ==============================


# Using "Date" class to represent a time index only 
# involving dates but not times within a day

# It is the recommended class for representing financial data 
# that are observed on discrete dates (e.g. daily closing prices)


?as.Date
# x         An object to be converted.
# format    A character string to specify the input format of the date


# create Date objects from a character string
# using function as.Date()
myDate <- as.Date("1970-01-01")
mode(myDate)
# [1] "numeric"
class(myDate)
# [1] "Date"


# internally it is number of days since Jan 01, 1970
as.numeric(myDate)
# [1] 0


myDates <- as.Date("2013-01-01", "2013-01-02")
myDates
# doesn't work with 2 variables, need to pass a vector


myDates <- as.Date(c("2013-01-01", "2013-01-02"))
as.numeric(myDates)

# argument format in as.Date()

as.Date("1970/01/01")
# [1] "1970-01-01"
as.Date("1970-01-01")
# [1] "1970-01-01"

as.Date("01/01/1970")
# [1] "0001-01-19"
# oops...


as.Date("1970-01-01", format = "%Y-%m-%d")
# [1] "1970-01-01"
as.Date("02/24/2015", format = "%m/%d/%Y")
# [1] "2015-02-24"


myDate
# [1] "1970-01-01"
format(myDate, "%b %Y %d")      # %b abbreviation of month
# [1] "Jan 1970 01"
format(myDate, "%B %Y %d")      # %B full month
# [1] "January 1970 01"




## standard date format codes
# Jan 23, 1990

# Code    Value               Example
# %d      Day of the month    23
# %m      Month               01
# %b      Month(abbreviated)  Jan
# %B      Month(full)         January
# %y      Year(2 digits)      90
# %Y      Year(4 digits)      1990


# using function class()
# recall that all Date objects are internally integers
myDate <- 20
class(myDate) = "Date"
myDate




## manipulating Date objects
rm(list = ls())

data <- read.csv("msft.csv", stringsAsFactors = F)
mode(data$Date)
# convert from character to class Date
myDates <- as.Date(data$Date)

# four default functions extracting parts of Date object (and POSIXt object)
months(myDates)     
quarters(myDates)
weekdays(myDates)
julian(myDates)

julian(myDates,origin = as.Date("2014-12-01"))# change origin

# arithmetic operations
myDate <- myDates[1]
myDate
# [1] "2014-12-31"
myDate + 1
# [1] "2015-01-01"
myDate - 1
# [1] "2014-12-30"
myDate > as.Date("2014-12-01")
# [1] TRUE





## creating date vectors using seq()
?methods
methods(seq)
methods(plot)


?seq.Date

# now create a date vector
myDateVec <- seq.Date(from = as.Date("2016-01-01"), to = as.Date("2017-01-01"), by = "day")
# equivalent to
# myDateVec <- seq(from = as.Date("2014-01-01"), to = as.Date("2015-01-01"), by = "day")
myDateVec <- seq.Date(from = as.Date("2014-01-01"), to = as.Date("2015-01-01"), by = "month")
myDateVec <- seq.Date(from = as.Date("2014-01-01"), to = as.Date("2015-01-01"), by = "2 months")


myDateVec


# alternatively
myDateVec <- seq(from = as.Date("2014-01-01"), by = "day", length.out = 365)

myDateVec <- seq(from = as.Date("2016-01-01"), by = "day", length.out = 365)# leap year!


myDateVec


######################In class exercise#################################################
# Assuming I want to create a vector of dates, How to find out the corresponding date is 
# weekday or weekend?



# ======================== POSIXt class ===================================
rm(list = ls())

# two POSIXt sub-class available in R:
# POSIXct 
# POSIXlt

# POSIXct represents date-time values as signed number of seconds
# since midnight GMT 1970-01-01
# can be regarded as an extension of Date class

# POSIXlt class represents date-time values a NAMED LIST with elements
# such as second, minute, hour, day, month, year, day of the year etc.


## creating POSIXct objects
# using as.POSIXct() functions
# the default format is "YYYY-mm-dd hh:mm:ss"

myDateTime <- as.POSIXct("2014-01-01 10:20:20")
myDateTime

?as.POSIXct

# POSIXct can be regarded as an extension of class Date
# We can format it as well
myDateTime1 <- as.POSIXct("01,01,2014 10:20:20", format = "%d,%m,%Y %H:%M:%S")
myDateTime1


# Difference from POSIXct and POSIXlt
myDateTime2 <- as.POSIXlt("01,01,2014 10:20:20", format = "%d,%m,%Y %H:%M:%S")

myDateTime1$sec
# Error: object 'myDatetime1' not found
myDateTime2$sec
# [1] 20


# please refer to the lecture notes to see the format codes


# creating POSIXct object from numeric variable
# The date which is assigned to origin will be treat as UTC/GMT time


num <- 0
myDateTime2 <- as.POSIXct(num, origin = "1970-01-01")
myDateTime2



# EST is 5 hours behind UTC
myDateTime3 <- myDateTime2 + 6*60*60
myDateTime3

# or we can set time zone when creating the object
myDateTime4 <- as.POSIXct(num, origin = "1970-01-01", tz = "UTC") # "GMT"
myDateTime4


# As Date objects, you can also use the "weekdays()", "months()", "quarters()", "julian()" on POSIXct objects
months(myDateTime1)
quarters(myDateTime1)
weekdays(myDateTime1)
julian(myDateTime1)

#Time Zone
#Without specification, the timezone for input will be the same as system time zone
#Time zone setting can be very tricky. E.X. the time zone for New York City can be either EST or EDT depends
#on the date
strptime("2013.12/23 01:00:34", "%Y.%m/%d %H:%M:%S", tz = "America/New_York")
strptime("2013.6/23 01:00:34", "%Y.%m/%d %H:%M:%S", tz = "America/New_York")


# strptime()
# The output of strptime is similar to as.POSIXlt
# The input of strptime is fixed as character type

dt1 <- strptime("2013.12/23 01:00:34", "%Y.%m/%d %H:%M:%S")
dt2 <- strptime("2013.12/23 01:02:37", "%Y.%m/%d %H:%M:%S")
dt1
dt2$sec

#You get time difference directly by using "-"
dt2-dt1
# Time difference of 2.05 mins

#use difftime can change unit
difftime(dt2, dt1, units = "secs")
# Time difference of 123 secs

# =================================Self-reading content: Package lubridate ==========================================

rm(list = ls())
library(lubridate)

## Parsing Dates and Times
# Instead of using as.POSIXct() and as.Date() to create date and time objects,
# you can use the smart parsing lubridate functions

# numeric argument
ymd(20130120)
mdy(01202013)
dmy(20012013)


# character strings as argument
mdy("Jan-20-2013")
mdy("January,20,2013")
mdy("01/20/2013")


# parsing date and time
mdy_hms("01202013103500")
mdy_hms("01/20/2013 103500")
mdy_hms("Jan 20 2013 10:35:00")


# The default time zone for this functions is UTC, 
# you can always change time zone using optional argument "tz = "
mdy_hms("Jan 20 2013 10:35:00", tz = "EST")



## Extracting Information

# Date Component      Extractor Function
# Year                year()
# Month               month()
# Week                week()
# Day of year         yday()
# Day of month        mday()
# Day of week         wday()
# Hour                hour()
# Minute              monite()
# Second              second()
# Time zone           tz()


# extracting
myDateTime5 <- mdy_hms(c("Jan 20 2013 10:35:00","Feb 20 2013 10:35:00"), tz = "EST")
# Note all below extracted numbers are in terms of EST
year(myDateTime5)
# [1] 2013
month(myDateTime5)
# [1] 1
hour(myDateTime5)
# [1] 10
second(myDateTime5)
# [1] 0


# modify object by the function
myDateTime5
month(myDateTime5) <- 10
myDateTime5

#=============================================================================================

# ContCompound() function allows the user to see the difference when change the yearly return
# into multi periods compounded return


result <- NULL
x <- 1:24  #Number of cuts
for(m in x){
  result <- c(result, ContCompound(0.1, m))
}

plot.ts(result, main = "Continuously Compounding",
        xlab = "Interest Payments", ylab = "Balance")


######################In class exercise#################################################
# Create the ContCompound() function which can generate the same plot as we have in the slides





#===============================Basic plots===========================================
#===============================scartter plot=========================================
#mtcars is a table already exsited in R, we will use this table to explain plot function
attach(mtcars)
mtcars
plot(mtcars$wt, mtcars$mpg)
#Running above command will get a scatter plot

plot(mtcars$wt, mtcars$mpg, xlab = "Weight", ylab = c("Mile per gallon"), main = "Scatter Plot")
#adding labels and title for a plot

plot(mtcars$wt, mtcars$mpg, pch=18, col= "red")
#change point type and color


#================================bar plot==============================================
mtcars$cyl
#[1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4
#number of cylinders in a car

barplot(mtcars$cyl) # a wrong expression


table(mtcars$cyl)
# 4  6  8 
# 11  7 14

barplot(mtcars$cyl)

barplot(table(mtcars$cyl), xlab = "Number of cylinders", ylab = "Frequency", col=c("red","blue","green"))
#change color for specific bar

#==============================stacked bar plot=====================================
table(mtcars$cyl, mtcars$gear) #table(value on Y-axis, value on X-axis)

#    3  4  5
# 4  1  8  2
# 6  2  4  1
# 8 12  0  2

barplot(table(mtcars$cyl, mtcars$gear), beside = T, 
        xlab = "Number of cylinders", 
        ylab = "Frequency", 
        col = c("red","blue","green"))
legend("topright", c("4", "6", "8"), fill = c("red", "blue", "green"),
       title = "Number of Cylindar")

#If beside = T, you will obtain a grouped barplot
#If beside = F, you will get stacked barplot


#=======================================Lengend==============================================
table=read.csv("C://Users//yzwar//Desktop//surgeplot//sandy_raw_forecast.csv")

plot(table$X,table$X1,type = "l")
lines(table$X2,type = "l", lwd=2, col="red")
points(table$X3, col="blue")

legend("topleft", c("forecast1", "forecast2", "forecast3"), fill=c("black","red","blue"))
legend("topright",c("forecast1", "forecast2", "forecast3"),
       col=c("black","red","blue"),
       pch=c(NA,NA,1),lty = c(1,1,NA),
       lwd=2,
       ncol = 3) #specify all legend will be in three columns

#===============================box plot=================================================
attach(ToothGrowth)
a <- ToothGrowth

#In boxplot, the x values should be a factor
boxplot(supp ~ len, data = ToothGrowth, xlab = "supply", ylab = "Tooth length", 
        border= c("blue","red"),  #change color for border
        col=c("green","blue")) #change color for box

par(mfrow=c(1,2))  # Arrange your plot by 1 row, 2 cols

boxplot(len ~ supp, data = ToothGrowth, xlab = "supply", ylab = "Tooth length", 
        border= c("blue","red"),  #change color for border
        col=c("green","blue")) #change color for box

boxplot(len ~ supp + dose, data = ToothGrowth, xlab = "supply + dose", ylab = "Tooth lenght",
        medcolor=rainbow(6), #change color for median value
        pch=17) # change outlier point style



#===============================histogram==================================================
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), #set up plot matrix, which is 2*2
       #c(1,1,2,3) means plot location will be 1,1,2,3
       widths=c(3,1), heights=c(1,2)) #for width, plot will be arranged as 3 units and 1 unit
#for height, plot will be arranged as 1 unit and 2 units
hist(wt)
hist(mpg)
hist(disp)




#===============================Advanced skill in plot======================================
#==============================Change X-axis description to time============================
table=read.csv("C://Users//yzwar//Desktop//surgeplot//sandy_obs.csv")
obs1=table[1297:1800,-1]

plot(obs1$time2,obs1$Surge2,type="l")
#x-axis is messed up due to too many points

timeline=strptime(obs1$time2, format = "%Y-%m-%d %H:%M")
plot(timeline,obs1$Surge2,main ="Sandy observation and ensemble based forecasts",
     ylab="Surge Height (meters)", xlab = "Time",
     type = "l", lwd=2, ylim=c(0,4))
#After we transfer time vector to time format, we are able to generate line plot
#But we still don't know what's the correct date


plot(timeline,obs1$Surge2,
     xaxt="n",         #deleting description on x-axis
     main ="Sandy observation and ensemble based forecasts",
     ylab="Surge Height (meters)", xlab = "Time",
     type = "l", lwd=2, ylim=c(0,4))
axis.POSIXct(side=1, at=cut(timeline, "days"), format="%m/%d/%Y") 


######################In class exercise#################################################
# Here is an example of creating a sactter plot
x <- seq(10,200,10)
y <- runif(x)

plot(x,y)
plot(x,y, xaxt="n")

#How to modify the labels on the X-axis? For example, instead of give 50, 100, 150, and 200
#labels, give me a new lable which shows 10, 20, 30, ..., 200


#===========================panel()=================================
# In plot(), it supports panel.first and panel.last
# panel.first   an 'expression' to be evaluated 
#               after the plot axes are set up but 
#               before any plotting takes place

# panel.last   an expression to be evaluated 
#              after plotting has taken place but 
#              before the axes, title and box are added.




Speed <- cars$speed
Distance <- cars$dist
plot(Speed, Distance, panel.first = grid(8, 8),
     pch = 0, cex = 1.2, col = "blue")


#==========================rect()====================================
#rect() allows the user to draw rectangel with given corrdinates, fill and borader colors
#This function can be used to create shaded area in a plot
rect(xleft, ybottom, xright, ytop, col)

rect(10, -5, 20, 100, col = 'gray')

#=========================ploygon()=================================
#polygon() draws the polygons whose vertices are given in x and y
#this function is an add-on functions to an existed plot

plot(x = c(4, 5, 6, 7, 8), y = c(2, 6, 4, 6, 2), xlim = c(1,10), ylim = c(1,10), col = "red")

x <-  c(4, 5, 6, 7, 8) # creating 2 lines with x value from 4:8 without y values
y <-  c(2, 6, 4, 6, 2) # for the first line, y will be assigned with initial value 2, last value 2
                       # for the second line, y will be assigned with 3, 6, 4, 6, 2

polygon(x, y, col = "orange", border = "red")

plot(x = c(4, 5, 6, 7, 8), y = c(2, 6, 4, 6, 2), xlim = c(1,10), ylim = c(1,10), col = "red")

x <- c(4,5,6) # creating 2 lines with x value from 4:8 without y values
y <- c(2,6,4) # for the first line, y will be assigned with initial value 2, last value 4
              # for the second line, y will be assigned with 2, 6, 4

polygon(x, y, col = "blue", border = "red")

plot(x = c(4, 5, 6, 7, 8), y = c(2, 6, 4, 6, 2), xlim = c(1,10), ylim = c(1,10), col = "red")

x <- c(c(4:8), rev(c(4:8))) #creating a circle, straing from 4, end at 8. 
                            #Then starting from 8, end at 4
y <- c(2,3,4,3,2,2,6,4,6,2) #creating 2 lines
                            #line 1: 2,3,4,3,2
                            #line 2: 2,6,4,6,2
                            #R will automatically decide the upper/lower boundary 


polygon(x, y, col = "orange", border = "red")

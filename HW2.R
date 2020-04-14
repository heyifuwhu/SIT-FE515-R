# ====================================================================================== #
# ===================================== FE515 HW2 ====================================== #
# ====================================================================================== #
#                                       Wang Yige                                        #
# ====================================================================================== #

# ====== Question 1 ===== #
# -------------------------------------------------------------------------------------- #
# 1. Explain what is dgeom() and pgeom().

# dgeom(x, p) returns the probabily density of a geometric distribution with x and p.
# where x indicates the number of failures and p indicates the probability of success.

# To clarify, dgeom() returns the result of P(x) = p(1-p)^x
# this P(x) shows the probability of failing x times and finally succeed for the first 
# time at trial (x+1), with the probability of a single success p. 

# pgeom() gives the distriubtion function, in other words, the cumulative probability.
# It returns the probability that you fail at most i times and succeed for the (i+1)th
# time with all 0 <= i <= x, and the probability of success for a single time is p.

# For example, if you toss a fair coin, which means the probability of getting a head
# is p = 0.5, then the probability of getting the first head with previous 2 tosses 
# being tail is dgeom(2, 0.5) = 0.125. And its quite intutive to get a result of 1/8.

dgeom(2, 0.5)

# On the other hand, if you toss this fair coin again and would like to calculate the 
# probability that you fail at most 2 times to get the first head, meaning the sum of all
# probabilities of dgeom(0, 0.5), dgeom(1, 0.5) and dgeom(2, 0.5), you will have to 
# compute pgeom(2, 0.5) = 0.875. This is the cumulative probability density.

pgeom(2, 0.5)

# -------------------------------------------------------------------------------------- #
# 2. Explain what is qgeom().

# The relationship of pgeom() and qgeom() is just like pbinom() and qbinom().
# They are, in some sense, inverse function, where pgeom() intakes number of failures 
# and output probability, while qgeom() intakes probability and outputs the smallest 
# number of failure that satisfies the distribution. 
# In an extreme case, for integer q, if pgeom(q, prob) = p, then qgeom(p, prob) = q.
# Generally, qgeom(p,prob) returns the smallest integer q which satisfies 
# pgeom(q, prob) >= p.

# For example, we just showed that pgeom(2, 0.5) = 0.875. So we know qgeom(0.875, 0.5)
# should be 2.

qgeom(0.875, 0.5) # = 2 

# Generally, for any pgeom(1, 0.5) < p <= pgeom(2, 0.5), qgeom(p, 0.5) should be 2.

pgeom(2, 0.5)      # The lower bound
pgeom(1, 0.5)      # The upper bound
qgeom(0.76, 0.5)   # It returns 2
qgeom(0.874, 0.5)  # It also returns 2
qgeom(0.75, 0.5)   # From here it returns 1

# For pgeom() and qgeom(). there are 2 optional arguments
# which are set default to be lower.tail = TRUE, log.p = FALSE
# For lower.tail, if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
# For log.p, if TRUE, probabilities p are given as log(p).

# -------------------------------------------------------------------------------------- #
# 3. Explain what is rgeom().

# rgeom() is quite similar to rbinom(), but with geometric distriubtion rather than
# binomial distriubtion. For rgeom(n,p), it returns n numbers, each number being  
# the number of failures before the first success of a random case, where the 
# probability of a single success is p.

# For example, rgeom(20,0.5) will generate 20 numbers. In the case of coin tossing, 
# this will generate results for 20 random cases. Each case you toss the coin until
# you see a head, and the corresponding number indicates the number of tails before
# you stop. 

rgeom(20,0.5)

# In this case, since the probability p is 0.5, it is very natural that you see a 
# head within 2 or 3 tosses. Even the outputs are random for each time, it is very
# rare to see any large numbers in your outcome (though technically you can have
# infinitely large numbers. You are then having very bad luck, or. very good luck
# in another sense). 

# However, if we change the condition to be, say, basketball shotting. You are a 
# bad shotter and the probability of a single independent successful shot is 0.1.
# Then rgeom(10,0.1) will show you 10 numbers of failures before you successfully
# make a shot.

rgeom(10,0.1)

# In this case, the numbers may vary largely. Sometimes you make the shot at the 
# first trial, so the outcome is 0. Sometimes it takes forever for you to make
# it so you have a very large number.

# Following cases shows the possible outcomes for different p
rgeom(15, 0.05)
rgeom(15, 0.95)
rgeom(15, 0.001)
rgeom(15, 0.999)
rgeom(15, 0) # This one is not valid, returning all NAs
rgeom(15, 1) # This one is valid, returning all 0s
rgeom(15, 0.4)
rgeom(15, 0.6)
rgeom(15, 0.618)
rgeom(15, 0.382)

# ====================================================================================== #
# ====== Question 2 ===== #

# It seems invalid to pre-set the probability of each draw and then calculate the 
# probability. Therefore, we set up our sample space then do everything practically.

red <- replicate(50, "red")
blue <- replicate(15, "blue")
yellow <- replicate(35, "yellow")
sample_space <- c(red, blue, yellow) 

# Now we have our sample space, which contains 50 red balls, 15 blue balls and 35 yellow 
# balls. We can start drawing balls.

# 1. Find the probability of seeing a yellow ball, with 10000 trials of taking one and 
#    putting back.

# The number of trials is 10000
no_trials <- 10000

# Set our result of observation with sample()
# replace = T indicates that we will put the ball back before we pick another one
observation <- sample(sample_space, no_trials, replace = T)

# To manipulate the data, we have to change them into dataframe
# We first change them into a table
observation_table <- table(observation)

# And then a dataframe
obs <- as.data.frame(observation_table)

# We can see the number of yellow balls through the dataframe obs
no_yellow <- obs$Freq[3]

# Thereforem the probability of seeing a yellow ball is:
p_yellow <- no_yellow / no_trials
p_yellow

# This result varies because it is generated by sample()
# But it should be around 0.35, which is quite intuitive

# -------------------------------------------------------------------------------------- #
# 2. Find the probability of seeing a red ball, without putting the picked ball back

# We know that the number of sample is 100
no_sample <- 100

# Now we obtain our observation generated by sample()
# but with replace = F, so that we don't put balls back this time
observation2 <- sample(sample_space, no_sample, replace = F)

# Then we would like to find the trend of seeing a red ball
# The idea is to check the probability of seeing a red ball at the "i"th pick
# As "i" increases and finally comes to 100, we can see the trend. 

# We first generate an empty set to hold our probabilities
p_red <- c()

# Then do the iteration, in each iteration, we basically do the same thing 
# as in the previous question
for (i in 1:100){
  
  # But we change the range of data to the first "i"th entries
  observation2_table <- table(observation2[1:i])
  obs2 <- as.data.frame(observation2_table)
  no_red <- obs2$Freq[2]
  
  # And get the probability of seeing a red ball after the "i"th pick
  p_red <- c(p_red, no_red / i)
}

# However, there are some NAs if the first several picks are not red balls
# We want to replace the NAs with 0
na = which(is.na(p_red))
for (i in na){
  p_red[i] <- 0
}

# Last step before plotting, we set P_red(0) = 0, which means that we don't 
# see any red balls if we don't pick any ball.
p_red <- c(0, p_red)

# Finally we can plot the data and find out the dynamics
plot(0:100, p_red, 
     main = "Probability of Seeing a Red Ball", 
     ylim = c(0,1),
     xaxt = "n",
     yaxt = "n",
     xlab = "Number of Balls Drawn from the Box",
     ylab = "Probability",
     type = "l",
     lwd = 2)
axis(side = 1, at = seq(from = 0, to = 100, by = 5))
axis(side = 2, at = seq(from = 0, to = 1, by = 0.25))
lines(0:100, replicate(101, 0.5), col = "red")
legend("topright", 
       c("Probability of Seeing a Red Ball", "P = 0.5"), 
       fill = c("black", "red"),
       title = "Probabilities")

# We can see from the plot that the probability of seeing a red ball p fluctuates sharply
# at the beginning, and then slowly clams down and finally converges to 0.5
# That is because at first the sample size is so small that the result of a single pick
# can influence the probability. After the sample size grows up, the influence of single
# pick becomes smaller and smaller. P will finally converge to 0.5, because there are 50
# red balls among a total of 100 balls. After picking up every single ball, we must have
# picked 50 balls so the probability will be 0.5.

# ====================================================================================== #
# ====== Question 3 ===== #
# -------------------------------------------------------------------------------------- #
# 1. Remove all rows which have "NA" values, then calculate mean value for each column

# import the lubridate library for later use
library(lubridate)

# Set path and read file
setwd("/Users/yigewang/Library/Mobile Documents/com~apple~CloudDocs/FE/FE515")
GOOGwNA <- read.csv("GOOGwNA.csv")

# Take out NAs with na.omit()
GOOG <- na.omit(GOOGwNA)

# Calculate mean value for seleceted columns
mean_value <- sapply(GOOG[2:7],mean)
mean_value

# -------------------------------------------------------------------------------------- #
# 2. Generate a plot using the high value and the low value columns

# Change the date values into standard form with mdy() in lubridate library
GOOG$Date <- mdy(GOOG$Date)

# Prepare the relevant elements used in plotting
# First we find the interval of the background
a <- as.Date("2017-7-1")
b <- as.Date("2017-10-1")

# And then we find the set of points used to draw a polygon
x <- c(GOOG$Date, rev(GOOG$Date))
y <- c(GOOG$High, rev(GOOG$Low))

# Now we can plot
# We draw a sketch first
plot(GOOG$Date, GOOG$High, 
     type = "l", 
     main = "Stock Price Trend of GOOG", 
     ylim = c(700,1300),
     xaxt = "n",
     yaxt = "n",
     xlab = "Date",
     ylab = "Stock Price")
axis.Date(side=1, at=cut(GOOG$Date, "months"), format="%Y/%m/%d")
axis(side = 2, at = seq(from = 700, to = 1300, by = 100))

# And put the background
rect(xleft = a, ybottom = 0, xright = b, ytop = 2000, col = "gray")

# Since it covers part of our plot, we have to plot them again
lines(GOOG$Date, GOOG$High, type = "l", col = "red", lwd = "2")
lines(GOOG$Date, GOOG$Low, type = "l", col = "green", lwd = 2)

# And fill in the space in between
polygon(x,y, col = "orange", border = NA)

# And add the legend
legend("bottomright", 
       c("High value", "Low value", "Difference in between", 
         "Data between July 1 and Oct 1"), 
       fill = c("red", "green", "orange", "gray"),
       title = "Legend")
# Done

# ====================================================================================== #
# ====== Question 4 ===== #
# -------------------------------------------------------------------------------------- #
# 1, Use the subset() to get a sub-table which contains only "Trade" data

# Read file
BA <- read.csv("BA.csv")

# And get the subset
BA_Trade <- subset(BA, BA$Type == "Trade")

# -------------------------------------------------------------------------------------- #
# 2. Combine "Date.L." and "Time.L.", then use strptime() to convert them to a time object

# Change the date column to a standard form using function ymd() from lubridate
# Then use paste() to combine the two columns
BA_Time <- paste(ymd(BA_Trade$Date.L.), BA_Trade$Time.L.)

# Set second digits to 5 to retain the structure for time
options(digits.secs = 5)

# Use strptime() to convert it into a time object
# And put it back to its original dataframe with an additional column
BA_Trade$Time <- strptime(BA_Time, format = "%Y-%m-%d %H:%M:%OS", tz = "America/New_York")

# -------------------------------------------------------------------------------------- #
# 3. Find out the last price in each minute

# For this question, the idea is to compare "minute" in each row
# If "minute[i+1]" > "minute[i]", then the "i"th price is what we want

# set an empty list to fill in our result
result <- c()

# Take care of the length of iteration, we stop at (length-1) because we are using (i+1)
for (i in 1:(length(BA_Trade$Time) - 1)){
  
  # Use minute() to get the minute element of each time entry
  if (minute(BA_Trade$Time[i+1]) > minute(BA_Trade$Time[i])){
    
    # Get the result and fit in our list 
    result <- c(result, BA_Trade$Price[i])
  }
}

# And don't forget there is one more left -- the last one
result <- c(result, tail(BA_Trade$Price,1))

# Now here is the final result
result
# ====================================================================================== #

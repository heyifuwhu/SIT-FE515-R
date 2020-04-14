# Agenda

# 1. Coin Flips
# 2. Generating Random Variables



# ===========================  Coin Flips ==============================
# Imagine that you are flipping a fair coin, up to 1000 times. 
# Print out the proporation of head after every flip.


# Analysis: 
# We could keep generating a logical variable, using 1 for heads and 0 for tails.
# We also need a vector, which has 1000 elements, recording how many heads we got after each iteration.
# Another vector can be used for recording 1000 probabilities.
# Functions will be used: \textit{sample(), plot()}.


# plot()
plot(x=, y=, )
plot(x = 1:10, y = 1:10)
x <- seq(-2, 2, by = 0.0001)
plot(sin(x), type="l")


# sample()
?sample
sample(x=, size=, replace=, prob=)
# sample with replacement and sample without replacement

sample(c(0,1), 1)

sample(x = c(1, 0), size = 1, replace = T, prob = c(0.5, 0.5))

# you can also use other functions, such as runif() + round()


No.heads <- 0
result.Vec <- NULL
for (flips in 1:1000)
{
  # x = c(1, 0), 1 means head, 0 means tail
  tmp <- sample(x=c(1, 0), size=1, replace=T, prob=c(0.5, 0.5))   
  # add tmp to number of head, if tmp = 1..., if tmp = 0, ...
  No.heads <- No.heads + tmp      
  result.Vec <- c(result.Vec,No.heads/flips)
}
plot(1:1000, result.Vec, type="l")     # produce a figure

No.heads <- 0
result.Vec <- vector()
for (flips in 1:1000)
{
  # x = c(1, 0), 1 means head, 0 means tail
  tmp <- sample(x=c(1, 0), size=1, replace=T, prob=c(0.5, 0.5))   
  # add tmp to number of head, if tmp = 1..., if tmp = 0, ...
  No.heads <- No.heads + tmp      
  result.Vec[flips] <- No.heads/flips
}
plot(1:1000, result.Vec, type="l")     # produce a figure

# other parameters in plot()

plot(1:1000, result.Vec, type="l", main="Coin Flips")
plot(1:1000, result.Vec, type="l", main="Coin Flips", ylim = c(0, 1))
plot(1:1000, result.Vec, type="l", main="Coin Flips", ylim = c(0, 1), 
     xlab = "Number of flips", ylab = "Probability")
# ...



# ===========================In class exercise==================================
# Assume I can draw a random number from a black box, the number I can draw are 1, 2, 3 with probablity 0.2, 0.5, 0.3
# What is the expectation of the value I can draw?
# How to check the value using sample() function?

exp <- 0
result <- NULL
for (i in 1:1000){
  temp <- sample(x=c(1, 2, 3), size=1, replace=T, prob=c(0.2, 0.3, 0.5))
  exp <- exp + temp
  result <- c(result, exp/i)
}
plot(1:1000, result, type="l")

sum(sample(x=c(1, 2, 3), size=10000, replace=T, prob=c(0.2, 0.3, 0.5)))/10000
mean(sample(x=c(1, 2, 3), size=10000, replace=T, prob=c(0.2, 0.3, 0.5)))


# ======================= Generating Random Variables =========================

?rnorm
# rnorm(n = , mean = , sd = )


x <- rnorm(n = 10000, mean = 0, sd = 1)
x
# computes a histogram
?hist
hist(x)
hist(x, nclass = 40)
hist(x, nclass = 40, main = "mu = 0, sigm = 1")

# another sigma
x <- rnorm(n = 10000, mean = 0, sd = 5)
x
hist(x, nclass = 40, main = "mu = 0, sigma = 5")


?set.seed

# Anywho the random numbers R gives you aren't really random. They're pseudo-random. 
# Basically there's a function that outputs numbers that look random. 
# To do this it needs some inputs. The first input it gets will be the 'seed'.


set.seed(1)
rnorm(5)
rnorm(5)

set.seed(1)
rnorm(5)



# other functions

# density function
dnorm(x = 0)    # mean = 0, sd = 1
dnorm(x = 1)    # check table if you want
for(x in seq(0, 1, by = 0.1)){
    print(dnorm(x))
}
x <- seq(0, 1, by = 0.1)
plot(dnorm(x),type="l")


# cumulative distribution function
pnorm(q = 0)
pnorm(q = 5)
for(x in seq(-3, 3, by = 0.1)){
    print(pnorm(x))
}
x <- seq(0, 1, by = 0.1)
plot(pnorm(x),type="l")

# other distributions
x <- rpois(1000, lambda = 2)
hist(x, nclass = 40)

x <- rexp(1000)
hist(x, nclass = 40)

x <- rt(1000, df = 10)
hist(x, nclass = 100)

# ==============================Binomial distribution==============================
# Only in discrete distribution, we can calculate the probability to see each event
# e.x. Pr(x=k) what is the probability to observe K success in N trails

?dbinom(x, size, prob, log = FALSE)
pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
rbinom(n, size, prob)
# We toss a fair coin throughout this section

# What is the probability to observe 2 heads in 5 tosses?
# Pr(x=2)
dbinom(x = 2, size = 5, prob = 0.5)  #Only assign the probality to observe a success
# [1] 0.3125

# What is the probability to observe at most 2 heads in 5 tosses?
# Pr(x<=2) = Pr(x=0) + Pr(x=1) + Pr(x=2)

px1 <- dbinom(x = 0, size = 5, prob = 0.5)
px2 <- dbinom(x = 1, size = 5, prob = 0.5)
px3 <- dbinom(x = 2, size = 5, prob = 0.5)

final.prob <- sum(px1, px2, px3)
final.prob
# [1] 0.5

dbinom(2, 5, 0.5) #an alternative expression
#Note: In R, pbinom or pnorm give you probablity for P(x<=k)
qbinom(0.5, 5, 0.5)
rbinom(10, 5, 0.5)

#====================================In class exercise============================================
###### what is the qbinom() and how it works?


#=============================Negative Binomial=========================================

#Negative binomial distribution is a discrete probability distribution of the number of successes 
#in a sequence of independent and identically distributed Bernoulli trials 
#before a specified (non-random) number of failure (denoted r) occurs
dnbinom(x, size, prob, mu, log = FALSE)
pnbinom(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
qnbinom(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
rnbinom(n, size, prob, mu)

#Assuming we are tossing a fair coin, what's the probability to observe the 2nd head at the 5th trial?

dnbinom(x = 3, size = 2, prob = 0.5) #x denotes the number of failure, size denotes the number of success


#What is the probability to have at most 3 failure?
pnbinom(q=3, size=2, prob = 0.5)

rnbinom(n = 10, size = 2, prob = 0.5)
# [1] 0 2 1 1 0 0 0 1 1 0


#=============================Homework=======================================
# Understand what is Geometric distribution and explain following functions

dgeom(x, prob, log = FALSE)
pgeom(q, prob, lower.tail = TRUE, log.p = FALSE)
qgeom(p, prob, lower.tail = TRUE, log.p = FALSE)
rgeom(n, prob)



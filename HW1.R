# ====================================================================================== #
# ===================================== FE515 HW1 ====================================== #
# ====================================================================================== #
#                                       Wang Yige                                        #
# ====================================================================================== #
# Question 2 #

# The idea is to calculate the lowest common multiple of the first two numbers
# and repetitively do that with the result and another number in sequence.
# To compute the lowest common multiple of two numbers,
# we first need to find out their greatest common divisor,
# then use their gcd to compute lcm, and lastly the result.

gcd <- function(a,b){         # gcd stands for greatest common divisor
  r <- a %% b                 # r is the remainder of a / b
  if (r){
    return (gcd(b,r))         # Euclidean methods of computing gcd
  } else {
    return (b)
  }
}

lcm <- function(a,b){         # lcm stands for lowest common multiple
  return (a * b / gcd(a,b))   # lcm equals the multiple of a and b divided by gcd(a,b)
}


equ <- function(a,b){         # after defining lcm, we calculate the lcm for all numbers
  result <- 1                 # in the sequence, by iterating this process, we get the 
  for (i in a:b){             # result
    result = lcm(result, i)
  }
  return (result)
}

# Tese case: 1-10 should give out 2520
equ(1,10)
# [1] 2520

# ====================================================================================== #
# Question 3 #

# Set the default path
setwd("/Users/yigewang/Library/Mobile Documents/com~apple~CloudDocs/FE/FE515")

# Read the table and name it
JPM2018 <- read.csv("JPM.csv")

# Then create a sub-table which only contains Open, High, Low, and Close, name it JPM_sub
JPM_sub <- JPM2018[2:5]

# Use the "sapply" function to calculate mean value for each column, and save it as "v"
v <- sapply(JPM_sub,mean)

# Use the "apply" function to calculate mean value for each row, and save it as "m"
m <- apply(JPM_sub,1,mean)     # MARGIN = 1 means by row
m <- matrix(data = m, nrow = 3, ncol = 5, byrow = TRUE,
       dimnames = NULL)        # To convert the data into requested matrix

# Check results
JPM_sub
v
m

# ====================================================================================== #
# Question 4 #

# 1. What's the difference between "mapply" and "lapply"?

# mapply is a multivariate version of sapply. 
# mapply applies FUN to each elements of each arguments, and returns a list.
# (or for SIMPLIFY = TRUE, a vector, array or list, similar to sapply)

# On the other hand, lapply takes only one list argument of the same length,
# applys FUN to each elements and returns a list.


# 2. How to use "mapply"? Write an example.

# For example, if we want to repeat a number "x" "t" times,
# and we have multiple pairs of such "x" and "t".
# we can take two lists of "x" and "t", and use mapply.
# e.g. mapply(rep, t = 1:4, x = 4:1)
# this will give out a list with the first element one "4", second element two "3"s,
# third element three "2"s, and forth element four "1"s.

mapply(rep, t = 1:4, x = 4:1)


# 3. Can you use "mapply" to the function you created in Question 2? 

# Yes. Take two lists of numbers and compute the corresponding result for each pair:
a <- c(1,2,3,4,5)
b <- c(9,8,7,6,5)
mapply(equ, a, b)
# the result should be [2520, 840, 420, 60, 5]

# ====================================================================================== #
# Question 5 #

# The idea of identifying prime number is to check if the number "n" 
# can be divided by any number "i" other than 1 and "n"

prime <- function(n){
  result <- T 
  if (n != 2){            # to avoid any problems caused by 2
    for (i in 2:(n-1)){
      if (n %% i == 0){
        result <- F
        break
      }
    }
  }
  return (result)
}
prime(23)
# 23 is a prime number
# ====================================================================================== #


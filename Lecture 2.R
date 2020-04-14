# Agenda : 
# 1. dataframes and factors
# 2. Import Data from CSV
# 3. Functions
# 4. R Coding Style

# ======================================= 1 Import/Export CSV =============================

#set file path
#getwd()
#setwd()

#additional code
#source()
#list.files()


read.csv(file=, header=, ...)
read.table(file=, header=, ...)

?read.csv

#by this command you would know your current/default path
getwd()
#by this command you can set up the path
setwd("C://Users//Yarch//Desktop//FE515 16F//Lectures//Week2_df_factors_functions")
#list all the files you have in this direction
list.files()



read.csv("goog.csv")
GOOG <- read.csv("goog.csv")

# GOOG is a list
mode(GOOG)
# GOOG is a dataframe
is.data.frame(GOOG)

head(GOOG)
names(GOOG)

GOOG$Adj.Close # the $ can Only be used when the data is dataframe, 

is.factor(GOOG$Date)
# [1] TRUE

GOOG <- read.csv("GOOG.csv", stringsAsFactors = F)
is.factor(GOOG$Date)
# [1] FALSE


# for export dataframe to CSV
?write.csv
GOOG_HEAD <- head(GOOG)
is.data.frame(GOOG_HEAD)

write.csv(GOOG_HEAD, file = "goog_head.csv")
rownames(GOOG_HEAD)
colnames(GOOG_HEAD)
write.csv(GOOG_HEAD, file = "goog_head.csv", row.names = F)

# ======================================= 2 Dataframe and factor =============================
## dataframe

# dataframe: a list of vectors of equal length

# two vectors
kids <- c("Joe", "Jill", "Mike")
ages <- c(11, 12, 8)

typeof(kids)
typeof(ages)

# create a list based on vectors
l <- list(names = kids, ages = ages)
l

mydataframe=data.frame(l)
data.frame(kids, ages)

# think about this question, can I cbind kids and ages together?

df <- data.frame(l)

# df is a list
df$names
df$ages

is.list(df)
# [1] TRUE
is.data.frame(df)
# [1] TRUE


## factor

# categorical variables, used in statistical modeling

x <- c(1,2,2,3,1,2,3,3,1,2,3,3,1)
f_x <- factor(x)
x
f_x
f_x_lable <- factor(x, labels = c("A", "B", "C"))
f_x_lable

# ======================================= 3 Missing Data =============================
##is.na() 
##is.nan()
##na.omit()

#creating a vector contains NA
x <- c(1,2,3,NA,NA,6,NA,8)
#checking which element is NA 
xna <- is.na(x)
xna

#call the elements which are not NA
x[!xna]

# =================================In class exercise============================
# 1. How to use which to find the location of NA elements?


# =======================4. Vectorized Operation =========================
  
  rm(list = ls()) #remove enviroment 
  
  # vectorized operation
  # will simplify your code and increase the performance
  
  x <- 1:4
  y <- 6:9
  
  # want to do x + y
  
  # using for loop
  result <- c()
  for (i in x)
  {
    result[i] = x[i] + y[i]
  }
  result
  
  
  # apply "+" on vectors
  result <- x + y
  result
  rbind(x, y, result)
  
  
  # another example
  x <- 1:4
  x > 2
  
  # make the result prettier
  rbind(x, x>2)
  
  
  # Q: How to show TRUE/FALSE in a similar table
  data.frame(x, x > 2)
  
  
  # more examples
  x + y
  x * y
  x / y
  x == y
  
  
  # vectorized operation on user defined functions
  mySquare <- function(x)
  {
    return (x^2+1)
  }
  
  
  x <- 1:4
  y <- 6:9
  
  mySquare(x)
  mySquare(y)
  
# ======================================= 5 User defined function =============================
  foo <- function(# parameters)   
    {
      # body
    }
    
    
    # keyword return
    foo <- function()
    {
      # body
      # a function without return
    }
    
    foo <- function()
    {
      # body
      return ()
    }
    
    
    # Example 1:
    PrintHW <- function()
    {
      "Hello World!"
    }
    PrintHW()
    
    
    PrintSomething <- function(sth)
    {
      print(sth)
    }
    PrintSomething("HW!")
    PrintSomething(1)
    
    
    
    
    PrintSomething <- function(sth)
    {
      flag = is.character(sth)
      if (!flag)
      {
        stop("Error: Parameter is not character!")  # a function to generate error message
      }
      print(sth)
    }
    
    
    PrintSomething("HW!")
    PrintSomething(100)
    
    
    
    # Example 2:
    add <- function(a, b)
    {
      c <- a + b
      return (c) #No output if you forget return()
    }
    add(1, 2)
    
    
    result <- add(1, 2)
    result
    
# =================================In class exercise==================================
    # Write a self-define function which can distingiush a input number is prime number or not.
    
# ========================6. apply functions ==========================
    
    lapply(X = ..., FUN = ...)
    sapply(X = ..., FUN = ...)
    apply(X = , MARGIN = , FUN = )
    
    # lapply takes two arguments: a list x, a function or a name of function.
    # If x is not a list, it will be coerced to a list using as.list().
    # lapply always returns a list, regardless of the class of the input.
    
    ?lapply
    
    
    mySquare <- function(x)
    {
      return (x^2)
    }
    
    
    x <- 1:10
    lapply(x, mySquare)
    
    # sapply will simplify the result of lapply if possible.
    sapply(x, mySquare)
    
    
    # more examples
    x <- list(rnorm(10000, mean = 0, sd = 1), runif(10000, min = 0, max = 1))
    a = lapply(x, mean)
    sapply(x, mean)
    
    x <- 1:4
    lapply(x, runif)
    
    # sapply will give the same result
    sapply(x, runif)
    
    ?apply
    # Returns a vector or array or list of values obtained by applying a function 
    # to margins of an array or matrix.
    aapl <- read.csv("AAPL.csv", stringsAsFactors = F)
    aapl <- head(aapl, n = 10)
    aapl
    apply(X = aapl, MARGIN = 2, mean)       # the first column is not numeric
    apply(X = aapl[-2], MARGIN = 2, mean)
    
    aapl
    aapl$Date <- NULL
    
    
    # lapply
    # sapply
    # apply
    # eapply
    # mapply
    # rapply
    # tapply
    
    
    # use apply functions will dramaticly increase the performance
    ?system.time
    
    x <- 1:10^5
    system.time(    
      for (i in x)
      {
        mySquare(x)
      }
    )
    
    # user  system elapsed 
    # 45.927   0.172  46.101 
    
    system.time(
      sapply(x, mySquare)    
    )
    
    # user  system elapsed
    # 0.200   0.002   0.203
    
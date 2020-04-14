# ================== Bisection Method==================================
# Finding the root for f(x) = x^2 - 2

x <- seq(-2, 2, by = 0.01)
fx <- function(x){
  y <- x^2 -2
  return(y)
}

#Based on the plot, you can see the shape of f(x)
plot(x, fx(x), type = "l")
abline(h = 0)

#set initial parameters
a <- -1.5
b <- -0.5
epslion <- 0.001


# demo code
while(1){
  c <- (a+b)/2
  f.c <- fx(c)
  
  f.a <- fx(a)
  f.b <- fx(b)
  
  if(f.c == 0 | abs(b-a) <= epslion){
    points(c, f.c, col = "red")
    print(paste("Root of f(x) is",  c))
    break
  }else{
    points(c, f.c, col = "red")
    if(f.a*f.c < 0){
      b <- c 
    }else{
      a <- c
    }
  }
}

#########################In class exercise############################
###Find the root of f(x) = 4x^3 - 2*x^2 +5 when -2 <= x <= 2
###Meanwhile, think about following questions:
###1. How to prevent infinity loop
###2. When setting the initial parameter a and b, how to prevent invalid input?

x <- seq(-2, 2, by = 0.01)
fx <- function(x){
  y <- 4*x^3 -2*x^2 +5
  return(y)
}

plot(x, fx(x), type = "l")
abline(h = 0)

a <- -1.5
b <- -0.5
epsilon <- 0.001
c <- (a+b)/2

while(1){
  c <- (a+b)/2
  f.c <- fx(c)
  
  f.a <- fx(a)
  f.b <- fx(b)
  
  if(abs(f.c) < 0.001 | abs(b-a) <= epsilon){
    points(c, f.c, col = "red")
    print(paste("Root of f(x) is",  c))
    break
  }else{
    points(c, f.c, col = "red")
    if(f.a*f.c < 0){
      b <- c 
    }else{
      a <- c
    }
  }
}


# ================== Newton's Method ==============================

# simple example, newton's method
# f(x) = x^2 - 2

fx <- function(x)
{
  y <-  x^2 - 2
  return (y)
}

# f'(x) = 2*x
dfx <- function(x)
{
  y <-  2*x
  return(y)
}

plot(dfx)

# initial plot
plot(fx, ylim = c(-3, 60), xlim = c(-8, 8))
abline(h=0, v=0)

x0 <-  8          # initial guess
epsilon <-  0.01  # convergent condition
points(x0, fx(x0), col = "red")

step <-  1        # step count

while(1)
{
  tmp <-  x0
  Sys.sleep(1)
  
  deltaX <-  fx(x0) / dfx(x0)
  x0 <-  x0 - deltaX
  points(x0, fx(x0), col = "red")    
  step <-  step + 1
  
  if (abs(x0-tmp) < epsilon)
  {
    points(x0, fx(x0), col = "red")
    Sys.sleep(1)
    print("Converged!")
    print(paste("x0 = ", x0, ", step = ", step, sep=""))
    break
  }
}


# ==================== calculate the bond yield ======================
rm(list = ls())

# a bond with coupon 3
# paid every 6 months
# face value 100
# bond price 98.39
# we want to calculate the yield

# 3 * exp(-0.5y) + 
# 3 * exp(-0.5y) + 
# 3 * exp(-0.5y) + 
# 103 * exp(-2y) = 98.39

bond <- function(y)
{
  value <- 3 * exp(-0.5 * y) + 
    3 * exp(-1 * y) + 
    3 * exp(-1.5 * y) + 
    103 * exp(-2 * y) - 98.39
  return(value)
}

dbond <- function(y)
{
  value <- -1.5 * exp(-0.5 * y) + 
    -3 * exp(-1 * y) + 
    -4.5 * exp(-1.5 * y) + 
    -206 * exp(-2 * y)
  return(value)
}

y0 <-  0.02
while(1)
{
  y1 <- y0 - bond(y0) / dbond(y0)
  if(abs(y0-y1) < 1e-5)
  {
    print("converged!")
    cat("y = ", y1, sep = "")
    break
  }
  y0 <- y1
}

#######################In class exercise##################################
#Redo the bond question. This time, you need to find the y using both bisection
#method and Newton's method
#Additionally, please add following features in both methods
#1. report how many steps are used when obtaining the final value
#2. compare the effieciency of both method. Instead of comparing the number of steps
# report the time length when running each method.

nm <- function()
{
  bond <- function(y)
  {
    value <- 3 * exp(-0.5 * y) + 
      3 * exp(-1 * y) + 
      3 * exp(-1.5 * y) + 
      103 * exp(-2 * y) - 98.39
    return(value)
  }
  
  dbond <- function(y)
  {
    value <- -1.5 * exp(-0.5 * y) + 
      -3 * exp(-1 * y) + 
      -4.5 * exp(-1.5 * y) + 
      -206 * exp(-2 * y)
    return(value)
  }
  
  y0 <-  0.02
  step.count <- 0
  while(step.count < 100000)
  {
    step.count <- step.count + 1
    y1 <- y0 - bond(y0) / dbond(y0)
    if(abs(y0-y1) < 1e-5)
    {
      print("converged!")
      cat("y = ", y1, sep = "")
      break
    }
    y0 <- y1
  }
  print(step.count)
  return()
}

bm <- function()
{
  bond <- function(y)
  {
    value <- 3 * exp(-0.5 * y) + 
      3 * exp(-1 * y) + 
      3 * exp(-1.5 * y) + 
      103 * exp(-2 * y) - 98.39
    return(value)
  }
  
  dbond <- function(y)
  {
    value <- -1.5 * exp(-0.5 * y) + 
      -3 * exp(-1 * y) + 
      -4.5 * exp(-1.5 * y) + 
      -206 * exp(-2 * y)
    return(value)
  }
  
  a <- 0.05
  b <- 0.10
  epslion <- 0.001
  step.count <- 0
  
  while(step.count < 100000){
    step.count <- step.count + 1
    c <- (a+b)/2
    f.c <- bond(c)
    
    f.a <- bond(a)
    f.b <- bond(b)
    
    if(f.c == 0 | abs(b-a) <= epslion){
      print(paste("Root of f(x) is",  c))
      break
    }else{
      points(c, f.c, col = "red")
      if(f.a*f.c < 0){
        b <- c 
      }else{
        a <- c
      }
    }
  }
  print(step.count)
  return()
}
nm()
bm()
start.time <- Sys.time()

cm <- function(n)
{
  return (2**n)
}
system.time(cm(1000000))

# ==========================Gradient descent========================================
# ============= Example 1: Simple gradient descent with 1 variable ================

rm(list = ls())

# function f(x) = x^2 - 10x + 3
# x.min = -b/2a = 5

fx <- function(x)
{
  y = x^2 - 10 * x + 3
  return (y)
}

# derivative of f(x)
# df = x^2 - 10

df <- function(x)
{
  y = x*2 - 10
  return (y)
}

plot(fx, xlim = c(4, 6))

# pseudo code:

# set initial
# set convergence condition
# loop: 
while(1)
{
  # algorithm
  # ...
  # if (converge)
  # {
  #     break
  # }
}




# initial points (guess)
x0 <-  6
points(x0, fx(x0), col = "red")

# step length
alpha <-  0.2

# condition to terminate the algorithm
epsilon <-  0.0001

# step count
step <-  1

while(1)
{
  cat("Calculating, step ", step, '\n', sep = "")
  # update x
  x1 <-  x0 - alpha * df(x0)
  
  # check convergence
  if (abs(fx(x1) - fx(x0)) < epsilon)
  {
    # converged, output some information
    cat("x = ", x1, '\n', sep='')
    cat("Final step: ", step, '\n', sep='')
    break
  }
  
  points(x1, fx(x1), col = "red")
  
  # update x
  x0 <-  x1
  step <-  step + 1
  #     Sys.sleep(1.2)
}


# ======================= Example 2: Local extrema ===================

rm(list = ls())

# f(x) = x * sin(x)

fx2 <- function(x)
{
  y <-  x * sin(x)
  return (y)
}


df2 <- function(x)
{
  y <-  sin(x) + x * cos(x)
  return (y)
}

plot(fx2, xlim = c(-20, 20), main = "f(x) = x*sin(x)")

plot(fx2, xlim = c(2, 13), main = "f(x) = x*sin(x)")

x0 <-  7.5    # try 2, 6, 7, 8
points(x0, fx2(x0), col = "blue")

alpha <-  0.1
epsilon <-  0.0001
step <-  1

while(1)
{
  x1 <-  x0 - alpha * df2(x0)
  if (abs(fx2(x1) - fx2(x0)) < epsilon)
  {
    cat("x = ", x1, '\n', sep='')
    cat("Final step: ", step, '\n', sep='')
    break;
  }
  points(x1, fx2(x1),  col = "red")
  x0 <-  x1
  step <-  step + 1
}
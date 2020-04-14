# ============================ Linear Regression ================================
# ============================ Simple example ===================================
x1 <- rnorm(100)
x2 <- seq(1:100)
y <- x1+x2

lm(y ~ x1+x2)
# Call:
#   lm(formula = y ~ x1 + x2)
# 
# Coefficients:
#   (Intercept)           x1           x2  
# 3.559e-14    1.000e+00    1.000e+00  
summary(lm(y ~ x1+x2))
# Coefficients:
#   Estimate Std. Error   t value Pr(>|t|)    
# (Intercept) 3.559e-14  2.843e-15 1.252e+01   <2e-16 ***
#   x1          1.000e+00  1.281e-15 7.805e+14   <2e-16 ***
#   x2          1.000e+00  4.918e-17 2.033e+16   <2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1


#============================= Linear Regression ================================
data <- read.csv("grape crops.csv", header=T)
yield <- data$yield
cluster <- data$cluster.count
plot(yield ~ cluster)

lm(yield ~ cluster)

lm.r <- lm(yield ~ cluster)
abline(lm.r, col='red')

summary(lm.r)

# #///////////////////////////////////////////////////////
# Call:
#   lm(formula = yield ~ cluster)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.60700 -0.19471 -0.03241  0.23220  0.64874 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.02790    0.78355  -1.312    0.219    
# cluster      0.05138    0.00725   7.087 3.35e-05 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.3641 on 10 degrees of freedom
# Multiple R-squared:  0.834,  Adjusted R-squared:  0.8174 
# F-statistic: 50.23 on 1 and 10 DF,  p-value: 3.347e-05

#====================additonal information========================
# Deleting alpha values

# Based on the p-value, intercept is not significant in our model.
newlm.r <- lm(yield ~ -1 + cluster)
summary(newlm.r)
# Call:
#   lm(formula = yield ~ -1 + cluster)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.66443 -0.23611 -0.04086  0.20604  0.71761 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# cluster 0.041956   0.001004    41.8 1.79e-13 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.3758 on 11 degrees of freedom
# Multiple R-squared:  0.9937,	Adjusted R-squared:  0.9932 
# F-statistic:  1747 on 1 and 11 DF,  p-value: 1.789e-13

plot(yield ~ cluster)
abline(lm.r, col = 'red')
abline(newlm.r, col = 'blue')



# (0, 0)
plot(yield ~ cluster, xlim = c(-10, 120), ylim = c(-5, 20))
abline(lm.r, col = 'red')
abline(newlm.r, col = 'blue')
abline(h = 0)
abline(v = 0)


# other functions:
coef(newlm.r)
resid(newlm.r)  # or
residuals(newlm.r)
fitted(newlm.r)

# ============================ Quadratic Model ================================

lm.q <- lm(yield ~ cluster + I(cluster^2))
lm.q
summary(lm.q)
fitted(lm.q)
data.frame(fitted(lm.q), yield)
plot(yield ~ cluster)
lines(sort(cluster), sort(fitted(lm.q)), type='l', col='red')




# ============================= Stock and ETF ================================

library(quantmod)
getSymbols("CSCO")
getSymbols("DIA")

csco <- data.frame(CSCO)
dia <- data.frame(DIA)

# get last price
csco.price <- csco$CSCO.Adjusted
dia.price <- dia$DIA.Adjusted


# get returns
csco.rtn <- diff(csco.price, lag = 1) / csco.price[-length(csco.price)]
dia.rtn <- diff(dia.price, lag = 1) / dia.price[-length(dia.price)]

plot(csco.rtn ~ dia.rtn)

lm1 <- lm(csco.rtn ~ dia.rtn)
abline(lm1, col = 'red')

summary(lm1)

lm2 <- lm(csco.rtn ~ -1 + dia.rtn)
abline(lm2, col = 'blue')
summary(lm2)


#============================= In class exercise==================================
# Download daily equity data of JPM and WFC and calculate daily return (either type)
# Build a linear model use WFC as explanatory variable and JPM as responce variable
# Write down the linear model and interpret the result. 

getSymbols("JPM")
getSymbols("WFC")

JPM <- data.frame(JPM)
WFC <- data.frame(WFC)

# get last price
JPM.price <- JPM$JPM.Adjusted
WFC.price <- WFC$WFC.Adjusted


# get returns
JPM.rtn <- diff(JPM.price, lag = 1) / JPM.price[-length(JPM.price)]
WFC.rtn <- diff(WFC.price, lag = 1) / WFC.price[-length(WFC.price)]

plot(JPM.rtn ~ WFC.rtn)

lm3 <- lm(JPM.rtn ~ WFC.rtn)
abline(lm3, col = 'red')

summary(lm3)

#============================= Mulitple regression================================
bone <- read.csv("biomark.csv")
# Variables:
#   VO+: a measure of bone formation
#   VO-: a measure of bone resorption
#   OC: a biomarker of bone formation
#   TRAP: a biomarker of bone resorption
  

bone.model <- lm(voplus ~ vominus + oc + trap, data = bone)

summary(bone.model)


# Call:
#   lm(formula = voplus ~ vominus + oc + trap, data = bone)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -364.19 -158.57  -15.13  120.08  441.11 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -243.4877    94.2183  -2.584  0.01549 *  
#   vominus        0.9746     0.1211   8.048  1.2e-08 ***
#   oc             8.2349     2.8397   2.900  0.00733 ** 
#   trap           6.6071    10.3340   0.639  0.52797    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 207.8 on 27 degrees of freedom
# Multiple R-squared:  0.8844,	Adjusted R-squared:  0.8715 
# F-statistic: 68.84 on 3 and 27 DF,  p-value: 9.031e-13

#======================= Stepwise selection ========================
# In R we use function step()
# By default this function uses the AIC "goodness" criterion.

null.model <- lm(voplus ~ 1, data = bone)
full.model.formula <- voplus ~ vominus + oc + trap

#====================Forward selection=======================
step(null.model, full.model.formula, direction = "forward")


#=================================== Step 1 ==========================
# Start:  AIC=395.48
# voplus ~ 1
# 
# Df Sum of Sq      RSS    AIC
# + vominus  1   8093909  1993152 347.21
# + trap     1   5901115  4185945 370.21
# + oc       1   4388785  5698276 379.77
# <none>                 10087061 395.48
# 
# Step:  AIC=347.21
# voplus ~ vominus


#=================================== Step 2 ==========================
# Df Sum of Sq     RSS    AIC
# + oc    1    809205 1183947 333.06
# + trap  1    463595 1529557 341.00
# <none>              1993152 347.21
# 
# Step:  AIC=333.06
# voplus ~ vominus + oc
# 
# Df Sum of Sq     RSS    AIC
# <none>              1183947 333.06
# + trap  1     17658 1166289 334.60
# 
# Call:
#   lm(formula = voplus ~ vominus + oc, data = bone)
# 
# Coefficients:
#   (Intercept)      vominus           oc  
# -234.144        1.019        9.404  


#====================Backward selection=======================
#wrong example
step(null.model, full.model.formula, direction = "backward")

#In backward stepwise selection, you should start with full model


step(bone.model, full.model.formula, direction = "backward")

#=================================== Step 1 ==========================
# Start:  AIC=334.6
# voplus ~ vominus + oc + trap
# 
# Df Sum of Sq     RSS    AIC
# - trap     1     17658 1183947 333.06
# <none>                 1166289 334.60
# - oc       1    363268 1529557 341.00
# - vominus  1   2797825 3964114 370.52

#=================================== Step 2 ==========================
# Step:  AIC=333.06
# voplus ~ vominus + oc
# 
# Df Sum of Sq     RSS    AIC
# <none>                 1183947 333.06
# - oc       1    809205 1993152 347.21
# - vominus  1   4514329 5698276 379.77
# 
# Call:
#   lm(formula = voplus ~ vominus + oc, data = bone)
# 
# Coefficients:
#   (Intercept)      vominus           oc  
# -234.144        1.019        9.404  

#====================Stepwise selection=======================
step(null.model, full.model.formula, direction = "both")

#=================================== Step 1 ==========================
# Start:  AIC=395.48
# voplus ~ 1
# 
# Df Sum of Sq      RSS    AIC
# + vominus  1   8093909  1993152 347.21
# + trap     1   5901115  4185945 370.21
# + oc       1   4388785  5698276 379.77
# <none>                 10087061 395.48

#=================================== Step 2 ==========================
# Step:  AIC=347.21
# voplus ~ vominus
# 
# Df Sum of Sq      RSS    AIC
# + oc       1    809205  1183947 333.06
# + trap     1    463595  1529557 341.00
# <none>                  1993152 347.21
# - vominus  1   8093909 10087061 395.48

#=================================== Step 3 ========================== 
# Step:  AIC=333.06
# voplus ~ vominus + oc
# 
# Df Sum of Sq     RSS    AIC
# <none>                 1183947 333.06
# + trap     1     17658 1166289 334.60
# - oc       1    809205 1993152 347.21
# - vominus  1   4514329 5698276 379.77
# 
# Call:
#   lm(formula = voplus ~ vominus + oc, data = bone)
# 
# Coefficients:
#   (Intercept)      vominus           oc  
# -234.144        1.019        9.404  

#=========================In class exercise==============================
# Use cheese.csv and answer following questions
# 1. which factor plays the most important role in cheese taste?
# 2. should I use all three variables when esitmating the cheese taste?

cheese <- read.csv("cheese.csv")

null.model <- lm(taste ~ 1, data = cheese)
full.model.formula <- taste ~  acetic + h2s + lactic 
step(null.model, full.model.formula, direction = "both")

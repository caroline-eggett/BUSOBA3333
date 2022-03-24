# Homework SP22 2nd No. 8 - Caroline Eggett

# Interpret beta in each of the following regression models.

#1a y = alpha + beta * x + epsilon

# For every one unit increase in x, y would increase by beta.

#1b log(y) = alpha + beta * x + epsilon

# One unit change in y will lead to a percentage change in beta.

#1c log(y) = alpha + beta * log(x) + epsilon

# For a percentage change in x will result in a percentage change in y.

#2 This question explores the consequences of omitted variable bias. First, define the
# "true" model as follows:
#     y = beta0 + beta1 * x1 + beta2 * x2 + epsiolon.

# Notice that the true model contains an intercept and two predictors. Use the code
# below to generate data according to this model.

set.seed(1)
n = 1000
error = rnorm(n)
rho = .9
Sigma = matrix(c(1,rho,rho,1),ncol=2)
X = matrix(rnorm(2*n),ncol=2) %*% chol(Sigma)
x1 = X[,1]
x2 = X[,2]
y = 2*x1 + x2 + error

#2a What is the correlation between x1 and x2 in your sample?

cor(x1, x2) #0.9033016

#2b Use the lm function in R to estimate the reduced" model
#     y = β0 + β1x1 + ε
# which ignores the effect of x2 on y. 
# Does omitted variable bias lead to an overestimate or underestimate of β1 in this case?

reg = lm(y~x1)
summary(reg)
# We do have bias (overestimate).  The estimate of x1 is 2.916, but the true value is 2.

#2c Provide a mathematical expression showing the bias of betahat1. (Hint: it will be a
# function of x1, x2, and beta2.)

x1 = as.matrix(x1)
x2 = as.matrix(x2)
bias = solve((t(x1)%*%x1) %*% (t(x1)%*%x2)) # omitted variable bias
  
#2d What is the bias of betahat1 in your sample?

# It is 9.41 x 10^-7

#2e In this example, the amount of bias is controlled by the correlation parameter
# rho that exists in all (-1, 1). For which value(s) of rho makes the omitted variable bias disappear?

plot(x1,x2)


set.seed(1)
n = 1000
error = rnorm(n)
rho = 0
Sigma = matrix(c(1,rho,rho,1),ncol=2)
X = matrix(rnorm(2*n),ncol=2) %*% chol(Sigma)
x1 = X[,1]
x2 = X[,2]
y = 2*x1 + x2 + error

plot(x1,x2)
cor(x1, x2) #0.02252166

reg = lm(y~x1)
summary(reg)
# The estimate of x1 is 2.028700 which is close to the true value of 2. A rho value of 0
# makes the omitted bias disappear.

#2f Run a second regression that includes both x1 and x2 as covariates. Show that
# you can now accurately estimate beta1.

set.seed(1)
n = 1000
error = rnorm(n)
rho = 0.9
Sigma = matrix(c(1,rho,rho,1),ncol=2)
X = matrix(rnorm(2*n),ncol=2) %*% chol(Sigma)
x1 = X[,1]
x2 = X[,2]
y = 2*x1 + x2 + error

reg = lm(y~x1+x2)
summary(reg)

#3 A 1993 government funded program in Michigan provided underprivileged students
# access to daily lunches for free. Suppose we want to understand the effect the lunch
# program has on student performance. In the MEAP data set, there is information on
# 408 Michigan high schools during the 1992-1993 school year. Let mathscore denote
# the percentage of students who passed the math portion of Michigan’s standardized
# exam (MEAP), and lunchprg denote the percentage of students enrolled in the lunch
# program at that school. 

# To understand the effectiveness of the lunch program, consider the following regression
# model.
#         mathscore = beta0 + beta1 * lunchprg + epsilon

#3a If the lunch program was effective, what sign should beta1 have?

# It should be positive, so it has a positive effect on the mathscore.

#3b Use the MEAP data set to estimate the regression model given above. Provide the
# model output below.

MEAP = read.csv('MEAP.csv')

reg = lm(MEAP$mathscore~MEAP$ï..lnchprg)
summary(reg)

# Call:
# lm(formula = MEAP$mathscore ~ MEAP$ï..lnchprg)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -24.386  -5.979  -1.207   4.865  45.845 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     32.14271    0.99758  32.221   <2e-16 ***
#   MEAP$ï..lnchprg -0.31886    0.03484  -9.152   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 9.566 on 406 degrees of freedom
# Multiple R-squared:  0.171,	Adjusted R-squared:  0.169 
# F-statistic: 83.77 on 1 and 406 DF,  p-value: < 2.2e-16

#3c Interpret the estimate of beta1. Do you believe the result?

# No.  This doesn't show us if the students DIDN'T have lunch.  It would be nice
# to have their existing scores, parental income, performances, etc.  We need more
# controllable variables in the analysis.

#3d Explain the results from (b) by discussing the relationship between lunchprg and
# epsilon (i.e., variables that affect y apart from lunchprg). Which assumption about
# regression models could be violated?

# There is a selection bias (stated above) by having students who participated
# normally would come from lower parental income.  To clarify in a mathematical expression,
# x (lunchprgm) is correlated with mathscore.

#3e Are there other variables in the MEAP dataset that should be included in the
# model?

# Some could include salary, drop rate, and graduation rate based on looking at the CSV file.

#3f Suggest a few possible omitted variables that could be driving the results of (c).

# Some could include existing scores, performances, etc.

#4 (Market Penalties for Earnings Warnings1) Is there a market penalty for being
#transparent about forthcoming earnings shortfalls? Many have argued that there is,
#citing lower average stock returns for firms warning investors compared to firms who
# do not. However, what if the firms with more unfavorable news are also more likely
# to issue warnings? This could happen if, for example, managers issue warnings to reduce the risk of shareholder lawsuits following large stock price declines or to mitigate
# reputational costs associated with withholding bad news. Consequently, the estimated
# "pendalty" for early warnings could be just be a result of selection bias.

# The earnings data set contains information on n = 1000 publicly traded companies. There are four variables included: returns measures the average stock return
# within a 5-day window after an earnings announcement; warning indicates whether
# the firm issued an early warning to investors; earningnews is an index measuring expected quality of firms’ earnings news; nonearningnews is an index measuring the
# expected quality of all non-earnings related news. Negative values of earningnews
# and nonearningnews can be interpreted as "bad news" while positive values can be
# interpreted as "good news."

# After downloading the data from Carmen, set the path (e.g., "/Users/smith.6588/Downloads")
# and load the data into R using load("path/earnings.RData").

load('earnings.RData')

#4a Ignore the potential selection bias and estimate the warning penalty by computing
# the difference in returns for those who warned and those who did not. Interpret
# your results.

reg = lm(earnings$returns ~ earnings$warning)
summary(reg)

# We learn that those that were warned, the returns go way down (by -10.9686).  Therefore,
# they shouldn't warn people.  HOWEVER, this probably isn't the only thing a person would
# be looking at.

#4b Are there differences in the average values of earningnews and nonearningnews
# for those who warned and those who did not?

t.test(earnings$earningnews[earnings$warning==1], earnings$earningnews[earnings$warning==0])
# mean of x mean of y 
# -1.415331 -0.8380145
#Therefore, we shouldn't warn.

t.test(earnings$nonearningnews[earnings$warning==1], earnings$nonearningnews[earnings$warning==0])
# mean of x  mean of y 
# -1.0229540  0.6100758
#Even though the news is unrelated to the news, there is still a negative effect on the return.

#4c Control for potential selection bias using propensity score matching (PSM). First,
# consider a model for whether a company decides to issue a warning.
# warning =
#  {1 if γ0 + γ1earningnews + γ2nonearningnews + η < 0
#    0 otherwise

# Use the following code to fit this model in R and then generate the associated
# propensity scores.

ps = glm(earnings$warning ~ earnings$earningnews + earnings$nonearningnews, family="binomial")
pscores = predict(ps, type="response")
cbind(earnings$warning, pscores)[1:10,]
# When propensity score is high, it means we have the earnings.  If propensity score high, warning = 1, otherwise
# warning = 0.
    
# By the propensity score theorem, the following regression model will accurately
# estimate the causal effect of issuing a warning.
  
# returns = alpha + theta* warning + phi * pscores + epsiolon
    
# Estimate this model and report the model output below.
    
reg = lm(earnings$returns~earnings$warning + pscores)
summary(reg)
# The warning did not actual explain the decrease in returns.  There are other
# factors that are impacting returns too!  Therefore, they should be honest and warn
# the people.

#4d In principle, if earningnews and nonearningnews are correlated with warning,
# then one could ignore PSM altogether and simply regress returns on warning,
# earningnews, and nonearningnews. This regression, if properly specified, would
# also accurately estimate the causal effect of a warning.

# However, a PSM approach to estimating theta is much more robust to potential
# misspecification of the model for returns. For example, the true model used to
# generate the earnings data set is of the form

# returns = alpha + theta * warning + beta1 * earningnews + beta2 * nonearningnews
#    + beta3 * earningnews2*I(earningnews < 0)
#   + beta4 * nonearningnews2*I(nonearningnews < 0) + epsilon.

# which adds an additional penalty for bad news.
# Suppose you did not know the true model, and instead fit the following simpler
# model

# returns = alpha + theta * warning + beta1 * earningnews + beta2 * nonearningnews + epsilon.
reg = lm(earnings$returns~earnings$warning+earnings$earningnews+earnings$nonearningnews)
summary(reg)

# Call:
#   lm(formula = earnings$returns ~ earnings$warning + earnings$earningnews + 
#        earnings$nonearningnews)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -80.977  -2.870   1.481   4.696  11.147 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              4.52438    0.11019  41.060   <2e-16 ***
#   earnings$warning         0.47639    0.22276   2.139   0.0325 *  
#   earnings$earningnews     3.60567    0.04422  81.536   <2e-16 ***
#   earnings$nonearningnews  2.03311    0.06515  31.206   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 7.187 on 9996 degrees of freedom
# Multiple R-squared:  0.5491,	Adjusted R-squared:  0.5489 
# F-statistic:  4057 on 3 and 9996 DF,  p-value: < 2.2e-16

# Discuss how the estimate of theta here differs from the PSM estimate in part (c).
# The value for theta in the PSM is 1.822 whereas the value using the above function
# gives thetha as 0.47639.  With that knowledge, we know that this is underestimating the value of
# the true parameter by a lot (it is either 1 or 0).  Due to this, we want to make sure
# we use the PSM for our regression.

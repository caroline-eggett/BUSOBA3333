# Homework SP22 2nd No. 7 - Caroline Eggett

#1. Let Yi denote the sales for individual i and Di denote a treatment indicator for whether
# individual i was exposed to an online advertisement. Refer to chapter 1 (pages 4-12)
# of Mastering ’Metrics to answer the following questions.

#1a. Sales given exposure to the advertising.

#1b Y0i is the potential sales for people that didn't see the ad.
# Y1i is the potential sales for people that did see the ad.

#1c. The potential difference of sales (the treatment effect).

#1d. Teh average sale amoutn people who didn't see the ad given that individual
# is assigned to the treatment group, the first one is not observable
# and the second one is observable.  The selection bias is the avg(Y0i | Di = 0).
# People that see the add would have want more.


#2. Suppose you have data x1,...,xn and y1,...,yn, where the x’s come from a population
# with mean μ1 and the y’s come from a population with mean μ2. A two-sample t-test
# is used to test the following hypothesis.
# H0 : μ1 − μ2 = 0
# H1 : μ1 − μ2 /= 0

x = rnorm(100)
y = 0.5 + rnorm(100)

my.ttest = function(x,y,alpha){
  nx = length(x)
  ny = length(y)
  ssqx = var(x)
  ssqy = var(y)
  diff = mean(x) - mean(y)
  sediff = sqrt(ssqx/nx + ssqy/ny)
  tstat = diff/sediff
  df = (ssqx/nx + ssqy/ny)^2 / (((ssqx/nx)^2/(nx-1)) + ((ssqy/ny)^2/(ny-1)))
  crtval = qt(1-alpha/2,df)
  pval = 2*pt(abs(tstat),df,lower.tail=FALSE)
  lower = diff - crtval*sediff
  upper = diff + crtval*sediff
    return(list(diff=diff,tstat=tstat,pval=pval,CI=c(lower,upper)))
}

my.ttest(x, y, 0.05)

t.test(x, y, alpha=0.05)

# Both these functions show the same results


#3 Generate data where selection bias is present.

set.seed(1)
n = 1000
error = rnorm(n)
x = rnorm(n)
t = ifelse(error+x<0,1,0)
y = 2*t + x + error

#3a. What is the true value of the treatment effect parameter used to generate your
# data?
# The true value is equal to 2.

#3b. In your data, does selection bias overestimate or underestimate the true treatment
# effect?
# The selection bias leads to an underestimate of the treatment variable as
# the mean(x) - mean(y) = 0.835 - 1.115 = -0.3196

t.test(y[t==1], y[t==0])

#3c. Explain your answer to part (b) by examining the relationship between the 
# treatment variable and the error term. (Hint: plot t against the error term.)

plot(t, error)
cov(t, error)

#3d. Which assumption about regression models is being violated here?
# The independent variable (t) is correlated with the error (x*error).
# Since the y is generated by variables that aren't independent, y is generated
# with selection bias.

#3e. Is there a difference in the average value of x between the treatment and control
# groups?

t.test(x[t==1], x[t==0])
# Based on the two-sample t-test, one can see significant differences (look at the p-value)
# in the covariate x for the treatment and control groups.


#4.Generate data where selection bias is NOT present. 

#4a. Modify the following line of code so that t is independently generated from a
# Bernoulli(p) distribution with p = 0.5.

t2 = rbinom(n, 1, 0.50)

#4b. Provide evidence that t is no longer correlated with the error term.
# The reason that t is now no longer correlated with an error term is because
# There is equal probability of t being a 1 or 0.

plot(t2, error)
cov(t2, error)

# The error is 0.004937 so there is no longer any correlation between t and
# the error term.

#4c. Show that randomization provides an accurate estimate of the true treatment
# effect.
y2 = 2*t2 + x + error

t.test(y2[t2==1], y2[t2==0])

# Since the difference is 1.995 - (-0.052) = roughly 2, we can verify
# that randomization provides an accurate estimate of the true treatment effect.

#4d. Is there still a difference in the average value of the x’s between the treatment
# and control groups?

t.test(x[t2==1], x[t2==0])

# Since the p-value is greater than 0.05 (p-value = 0.268), there is not a significant
# difference between the treatment and control group.

#5 Download and load the data.
load('airbnb.RData')

#5a. What are the booking rates for the treatment and control conditions?
# For the control group, the booking platform is 0 which stands for the old search page.
# For the treatment group, the booking platform is 1 which stands for the redesigned page.

# If a user purchased a booking during their session, the book is 1.  If a user did not 
# purchase a booking during their session, the book is 0.

#5b. Formally test for differences in booking rates between the two platforms. Include
# the results of the test below.
platform = airbnb$platform
book = airbnb$book
t.test(book[platform==1], book[platform==0])

#5c. What do you conclude from the analysis in part (b)?
# Based off the independent two-sample t-test, there is a significant difference
# between those those that use the old platform and buy and those that use the new
# platform and buy.  We know this is significant as the p-value is 0.01623 < 0.05
# (a 95% confidence interval was chosen).

#5d. Based on your reading of the article, are there other variables you would include
# in your analysis to better understand how the new platform affects booking rates?
# I would like to be able to see the Internet browser used since that seemed to impact
# the p-value in Airbnb's experiment.
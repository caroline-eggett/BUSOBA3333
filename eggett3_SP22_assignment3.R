# Homework SP22 2nd No. 9 - Caroline Eggett

#1
#1a Explain why it would not be a good idea to simply compare the average movie
# attendance for members and non-members (including all freshmen).

#1b Explain the two criteria that Zi, as defined above, must satsify to be considered
# a “valid” instrument.

#1c Explain the concept of a local average treatment effect (LATE) in the context of
# this problem.

#2 Consider the system of equations:
#     y = beta0 + beta1*x + epsilonY
#     x = alpha0 + alpha*z + epsilonX
#
# where (epsilonY, epsilonX) ∼N(0,Σ), y is a dependent variable, x is an endogeneous predictor, and
# z is an instrumental variable. Generate data from the model above using the following
# code.

set.seed(1)
n = 1000
rho = .9
Sigma = matrix(c(1,rho,rho,1),ncol=2)
error = matrix(rnorm(2*n),ncol=2) %*% chol(Sigma)
ey = error[,1]
ex = error[,2]
z = runif(n)
x = 4*z + ex
y = 1 - 2*x + ey

#2a Use the lm function and estimate a regression of y on x alone (which ignores the
# endogeneity of x). What is the estimate of beta1?

#2b Which assumption about regression models is violated by running the model in
# part (a)?

#2c Using your data, show that z satisfies both the relevance assumption Cor(z,x) 6= 0
# and the exlcusion restriction Cor(z,epsilonY) = 0. The standard error of a correlation
# coefficient is 1√n where n is the sample size.

#3 Write a function in R that implements a two-stage least squares (2SLS) estimator. The
# input of the function should be the dependent variable y, the endogeneous predictor x,
# and the instrumental variable(s) z. The output of the function only needs to include
# the point estimate of the coefficient on x.

#3a Copy and paste your code below.

#3b Load the AER package in R and show that your function produces the same results as the ivreg function (you only need to show the equivalence of the point
# estimates). You can do this using the simulated data (y,x,z) from Problem 2 using the command
#     ivreg(y ~ x | z).

#3c Suppose you could not observe z, but instead observed another instrument z′ that
# is generated as follows.
#     zprime = z + rnorm(n,sd=10)

# Is z′ still a valid instrument? Provide evidence using your data.

#3d Rerun the IV regression (with ivreg) using z′ instead of z. What is the effect on
# your inference for beta1?

#4 Family CEOs and Firm Performance1) Companies controlled by founding families, so-called “family firms,” continue to play a major role in the global economy. A
# recent article in The Economist 2 reports that family firms actually constitute 19% of the Fortune Global 500. One of the more challenging issues faced by these firms relates
# to CEO succession decisions: should the family continue to hire within or bring in an
# outsider?

# Measuring the effects of these decisions on firm performance is challenging, given that
# factors entering the choice of CEO could also relate to firm outcomes. For example,
# a firm may appoint a family CEO if it is doing well, but look to an outside hire if
# it is struggling and in need of drastic change. Instrumental variable methods can be
# used to isolate the causal effect of family CEOs on firm performance, provided a valid
# instrument is available. The proposed instrument is the gender of the departing CEO’s
# first-born child.

# The familyceo data set provides information on a cross-section of n = 5000 firms
# within the same industry. The following variables are included: oroa is the percent
# change in operating return on assets within a two-year window surrounding a CEO
# transition, famCEO is equal to 1 if the firm decided to hire within the family and 0
# otherwise, and genderfirst is equal to 1 if the departing CEO’s first-born child is male
# and 0 otherwise.

# After downloading the data from Carmen, set the path (e.g., "/Users/smith.6588/Downloads")
# and load the data into R using load("path/familyceo.RData").

#4a Ignore the potential endogeneity of the famCEO variable and estimate the fol-
# lowing regression:
#     oroa = beta0 + beta1*famCEO + epsilon.

# What is the estimated effect of family CEOs on firm performance here?

#4b Do you think it is reasonable to assume that Cov(famCEO,ε) = 0? If not, what
# sign would you expect the covariance to have?

#4c Based on your reading of the paper (e.g., pages 662-663), provide a justification for
# why genderfirst is a valid instrument for famCEO. Include empirical evidence
# when possible.

#4d Use two-stage least squares to estimate the effect of family CEOs on firm performance, using genderfirst as an instrument for famCEO. Interpret the causal
# effect, and compare to your answer from part (a). If interested, refer to pages
# 663-673 in the paper to follow a similar analysis.


# Homework SP22 2nd No. 9 - Caroline Eggett

# Differences in Differences

# Consider the following regression model
#     y = alpha + beta*treat + gamma*post + sigma*(treat ∗post) + epsilon
# where treat is a dummy variable indicating the presence of a treatment, post is a
# dummy variable indicating the post-treatment time period, and (treat*post) is an
# interaction term. Generate data from the model above using the following R code.

set.seed(1)
n = 1000
error = rnorm(n)
treat = rbinom(n,1,.5)
post = rbinom(n,1,.5)
y = treat + 2*post + 4*treat*post + error

# 1a Use the following code to make a scatter plot of the dependent variable y against
# the post indicator variable. Include your figure below.

plot(jitter(post[treat==0],.5),y[treat==0],ylim=c(min(y),max(y)),
     xlab="post",ylab="y",xaxt="n",pch=16)
axis(1,at=c(0,1),labels=c(0,1))
points(jitter(post[treat==1],.5),y[treat==1],col=2,pch=16)
legend("topleft",c("control","treatment"),bty="n",pch=16,col=1:2)

# 1b What is the true value of the treatment effect parameter used to simulate your
# data?

# The true value of the treatment effect is 4.

# 1c Write one line of code to estimate sigma using the definition of a difference-in-differences
# (DD) estimator. Report your code and the estimate of sigma.

(mean(y[treat == 1 & post == 1]) - mean(y[treat == 1 & post == 0])) - 
  (mean(y[treat == 0 & post == 1]) - mean(y[treat == 0 & post == 0]))

# 1d Now estimate sigma using the lm function to estimate
#   y = alpha + beta*treat + gamma*post + sigma*(treat*post) + epsilon.
# Compare the estimate of sigma here to what you found in part (c).

reg = lm(y ~ treat + post + (treat*post))
summary(reg)
# The interaction variable coefficient is the same as the output in part c.

# 2 (Measuring the Impact of “Buy Online Pickup in Store”1) Suppose you are
# working as an analyst for Lowes, who is thinking of adding a “buy online pickup in
# store“ (BOPS) functionality. In the BOPS system, a customer will search and purchase
# products using the online store (where the transaction is counted as an online sale),
# but then go to a nearby brick and mortar store for pickup. Given the general decline
# in traffic to brick and mortar retail stores, Lowes is concerned that adding BOPS will
# do more to hurt, rather than help, in-store sales.
# The company recently acquired a data set about a competitor, The Home Depot, who
# implemented BOPS last year. The Home Depot has hundreds of brick and mortar
# retail stores in the US and in Canada, but they only enabled the BOPS option in the
# US and not Canada. The management team at Lowes is hoping that the analysis of
# The Home Depot’s data will inform Lowes decision on whether to adopt BOPS.
# The homedepot data set contains n = 2000 observations of monthly store sales for 50
# stores in the US and 30 stores in Canada. The data reports monthly sales for a one
# year period. Halfway through the sample period is when The Home Depot began to
# offer the BOPS option to the US stores. The variable store is an arbitrary store ID
# number, logsales reports the log of total monthly sales (in US dollars), US equals 1
# if the store is in the US and 0 if it is in Canada, and after indicates whether the
# observation corresponds to the period after BOPS implementation.

# PLEASE NOTE: LOGSALES WILL BE A PERCENTAGE CHANGE!

# After downloading the data from Carmen, set the path (e.g., "/Users/smith.6588/Downloads")
# and load the data into R using load("path/homedepot.RData").

load('homedepot.RData')
attach(homedepot)

# 2a In the context of this problem, what assumptions need to be made for a difference-
# in-differences approach to be appropriate for estimating the effect of BOPS on
# in-store sales? Do these assumptions seem reasonable?

# Homedepot is a competitor of Lowes, they have very similar patterns.
# First half of year no differences between US and Canada, last half of year there is.
# The assumption is that overtime, there are no differences between the control and treatment.
# Also, need to assume that the US and Canada have similar increases in sales since they have the
# same marketing techniques.  Furthermore, during the year, there will be no systematic differences.

# 2b Based on your reading of the article (see sections 1 and 2), what other factors in
# addition to effects on brick and mortar sales should Lowes consider when deciding.

# Other factors could include cyclical cycles like holiday seasons (Christmas), 
# inventory issues, or the use of technology (can people afford, what ages use, etc.)
# .  In order to make sure these don't affect the treatment, we need
# need to include these as other variables (the control variable).

# 2c Suppose you ignored the natural experiment that occurs in the data and decided
# to only look at the change in sales after implementing BOPS for US stores. What
# would you conclude about the effectiveness of the BOPS system?

summary(lm(logsales ~ after, data = homedepot[US==1,]))

# There are unobserved variances that will not be considered.  By imposing the BOPS system,
# it will decrease the sales by 20% and conclude this is not a good idea.

# 2d To estimate the causal effect of BOPS on in-store sales, estimate the following
# model (notice the similarity to eq. 4 in the paper):
#   log(sales) = alpha + beta*US + gamma*after + sigma*(US ∗after) + epsilon.
# Report your model output below.

summary(lm(logsales~US + after + (US*after)))

# This question is based on the published paper “Integration of Online and Offline Channels in
# Retail: The Impact of Sharing Reliable Inventory Availability Information” which is available at
# http://pubsonline.informs.org/doi/abs/10.1287/mnsc.2014.1951. The data used in this question are simulated and roughly based on the results presented in the paper.

# 2e Interpret the estimate of sigma.

# Sigma is 0.104095.  This means that by implementing the BOPS system, it increases
# sales by 10.415, concluding that it is a good idea to implement the system.

# 2f Based on your results, would you advise Lowes to adopt the BOPS system?

# Yes, I would recommend that Lowes implement the system.

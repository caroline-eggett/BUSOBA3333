# Homework No. 5 - Caroline Eggett

#1. Please load the Ford data (e.g., RData_FordData.RData) in to your R console using 
# the following command. 

load('Ford Data v2/RData_FordData.RData')
library(bayesm)

Data.Main[[1]]$X
Data.Main[[1]]$y

#2. Please read the cluster data (Cluster.csv) you have created in Assignment #2 
# using the following R command.
# the nclus is NOT defined

clust = as.matrix(read.csv('Cluster.csv', sep=',', header=TRUE), ncol=4)

#3. Take the “Driving Experience” as the based group, and run the provided R 
# command to produce a part-worth matrix of each segment. 

set.seed(53)
Prior <- list(ncomp=1) 
keep <- 10 
Mcmc <- list(R=40000,keep=keep) 
z = clust 
z = t(t(z) - apply(z,2,mean))  
Data_M <- list(p=6,lgtdata=Data.Main, Z=z[,c(2:4)])               
# Driving Experience as based group 
out.Main <- rhierMnlRwMixture(Data=Data_M,Prior=Prior,Mcmc=Mcmc)  
# Run HB estimation 

#### Calculate utility mean for each segment ###  
# set burnin 
lbound = 2000 
ubound = 4000 
nvar <- 26
nclust <- 4
# cluster mean - driving experience (based group) 
DrivExp = matrix(NA, nrow = lbound, ncol = nvar) 
for (itr in (lbound+1):ubound) { 
  DrivExp[itr-lbound, ] = (out.Main$nmix$compdraw[[itr]][[1]]$mu)   
} 
DrivExp = colMeans(DrivExp) 
DrivExp = round(DrivExp,2) 

# cluster mean - account for cluster deviation 
betabarind = array(NA, dim = c(lbound, nhh, nvar, (nclust-1) ) ) 
for (itr in (lbound+1):ubound) { 
  for(ind in 1:nhh){ 
    mu = out.Main$nmix$compdraw[[itr]][[1]]$mu 
    diviate = (matrix(out.Main$Deltadraw[itr,],ncol=(nclust-1) ) 
               * matrix(c(as.vector(clust[ind,2:4])),ncol = (nclust-1), nrow = 
                          nvar, byrow = TRUE)) 
    betabarind[itr-lbound,ind,,] = mu + diviate 
  } 
} 
betabarmean = array(NA, dim = c(nhh, nvar, (nclust-1) ) ) 
for(g in 1:3){ 
  for(ind in 1:nhh){ 
    betabarmean[ind,,g] = colMeans(betabarind[,ind,,g]) 
  } 
} 

# cluster mean - Hauler 
Hauler = colMeans(betabarmean[clust[,2]==1,,1]) 
Hauler = round(Hauler,2) 
# cluster mean - BadGM 
BadGM = colMeans(betabarmean[clust[,3]==1,,2]) 
BadGM = round(BadGM,2) 
# cluster mean - NeedAssis 
NeedAssis = colMeans(betabarmean[clust[,4]==1,,3]) 
NeedAssis = round(NeedAssis,2) 

cmean = rbind(DrivExp, Hauler, BadGM, NeedAssis) 
colnames(cmean) = colnames(Data.Main[[1]]$X) 
cmean = t(cmean) 
print(cmean)

# save cmean to a .csv

write.csv(cmean, 'cluster mean.csv')

#4. Answer the following questions

#4a. Which brand is the most preferred brand by each segment?

# Driving Experience: Toyota
# Bad_GM: Toyota
# Hauler: Honda
# Need Assistance: Honda

#4b. Which product features are relatively important for each segment?
# (i.e. part-worth > 1.00)

# Driving Experience: Rear obstacle warning is an important feature.  This could
# be due to this segment valuing technology driving consistance and the use of technology
# to keep a driver safe.

# Bad_GM: Rear cross traffic collision warning, rear obstacle warning, surround 
# view monitor, reverse collision avoidance, and automatic parking are important
# features.  This is due to other frustrations individuals in Bad_GM have (these
# are not listed in the analysis).

# Hauler: Rear cross traffic collision warning, rear obstacle warning, and reverse 
# collision avoidance are important features.  This is due to the segment enjoying
# getting off the road--they want to make sure what they do is safe off-road.

# Need Assistance: Premium stereo and forward collision avoidance are important 
# features. This segment is motivated by the ability to hear others in the car and
# have a safe car due to how cautious they are.

#4c. Which product features are less preferred by each segment?
# (i.e., negative part-worths)

# Driving Experience: Drive train is the least preferred feature.

# Bad_GM: Drive train, carpeted floor mats, forward collision warning, 
# adaptive cruise control, and lane keep assist are the least preferred product features.
# This is probably motivated by their desire for good gas mileage--they don't necessarily
# have interest in other perks of the car.

# Hauler: Drive train, carpeted floor mats, forward warning collision, and adaptive 
# cruise control are the least preferred product features.  This is due to their
# motivation to go off-road and haul large items.  They don't need a nice interior,
# nor do they need technology features that hinder and off-road experience.

# Need Assistance: Drive train is the least preferred product feature.

#4d. Are respondents in this segment sensitive to the price change?

# For all the segments, there was relatively little important (negative values) for the price.

#Answers to question #5 are within question #4.
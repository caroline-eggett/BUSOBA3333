# Homework No. 2 - Caroline

setwd("/Users/carol/OneDrive - The Ohio State University/2022 Spring/BUSOBA 3333/Ford Data v2")

#1a
RNGkind(sample.kind = "Rounding")
set.seed(53)
## Read Ford Data ##
data = read.csv("Ford_Data.csv", sep=",", header=TRUE)
varname = colnames(data)
colnames(data) = varname

#1b
motivations = data[,grepl("Q2_|Q3_", names(data))]

#1c
nhh = dim(motivations) # There are 33 motivation variables.

#1d
freq = apply(motivations, 2, FUN = sum)

#1e
m = freq / nrow(motivations)

#1f
order(m, decreasing = TRUE)

# index 16: Comfortable 75.9%
# index 7: Handlecond 56.9%
# index 14: Tech Defensive 49.7%

#2i
out = hclust(dist(motivations), method="complete") 
plot(out,main="Complete Linkage", xlab="", sub="", cex =.9) 

#2bii
nclust = 10 #confused on how to find this answer
FA = factanal(motivations, nclust, rotation="varimax") 
print(FA) 

#2biii
# Not sure what I need to look at.  Email

#2biv
# Not sure what I need to do here too

#3a
# What is their cut point?
F1 = FA$loadings[,c(1)]*(abs(FA$loadings[,1])>=p1) 
F2 = FA$loadings[,c(2)]*(abs(FA$loadings[,2])>=p2) 
F3 = FA$loadings[,c(3)]*(abs(FA$loadings[,3])>=p3) 
F4 = FA$loadings[,c(4)]*(abs(FA$loadings[,4])>=p4) 
AdjFloading = cbind(F1, F2, F3, F4) 

#3b
score = as.matrix(Motivations) %*% AdjFloading

#3c
stdscore = log(score)

#4a
cluster = kmeans(score, centers = nclust)

#4b
print(cluster)

#4c
# Interpret the cluster results and name each cluster.
# Please state your reasons.

#5a not sure how to do
group = 
  
#5b
  clust = matrix(0, ncol=nclust, nrow = nhh) 
for(i in 1:nhh){ 
  clust[i,group[i]]=1 
}

#5c
write.csv(clust, sep=",")

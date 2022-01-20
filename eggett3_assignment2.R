# Homework No. 2 - Caroline

setwd("/Users/carol/OneDrive - The Ohio State University/2022 Spring/BUSOBA 3333/BUSOBA3333")

# 1a. Please read the data (Ford_Data.csv) in to your R console using the following 
command. 
RNGkind(sample.kind = "Rounding")
set.seed(53)
## Read Ford Data ##
data = read.csv("Ford_Data.csv", sep=",", header=TRUE)
varname = colnames(data)
colnames(data) = varname

# 1b. Create a subset Motivations by selecting the following variables of Motivations 
# using grepl(). 
motivations = data[,grepl("Q2_|Q3_", names(data))]
motivations = motivations[,-c(19,33)] #deleting b/c in questionnaire asking to put another reason
colnames(motivations) = c("trans", "roadtrip", "rackuser" , "haul",  "command", 
                          "reliable",  "handlecond",  "concern",  "adjslow",  
                          "expressme", "lookright",  "dontcarecar",  "tech_driveconsist",  
                          "tech_defensive",  "hearothers", "confortable",  "panoramicview",  
                          "other", "bad_GM",  "highbill",  "difficulttodrive",  "notcomfortable",  
                          "hassletoshare", "difficulttoload",  "dontlikecoldcar",  "likelytogetlost",
                          "notenoughroom",  "notenoughtowcap", "notenoughstable",  
                          "notenoughhauling",  "other_frus")

# 1c. Create a variable nhh that indicates the sample size of Ford dataset.
dim(data)
nhh = dim(data)[1] # There are 547 observations

# 1d. Create a vector freq that indicates the frequency of each motivation being check 
# off. 
freq = apply(motivations, 2, FUN = sum)
print(freq)

# 1e. Let m be the motivation Prm = Freqm / nhh
cprob = freq / nhh

# 1f. What are the top 3 motivations for consumer to purchase an SUV?
ranking = cprob[order(cprob, decreasing = TRUE)]
print(ranking)

# Comfortable 75.9%
# Handlecond 56.9%
# Tech Defensive 49.7%

# 2a. Use the provided R command to determine how many potential clusters are there 
# in the Motivation subset.
out = hclust(dist(motivations), method="complete") 
plot(out,main="Complete Linkage", xlab="", sub="", cex =.9) 

# 2bi. Create a variable nclust that determine the number of factors using your 
# answer from a.
nclust = 4

# 2bii. Use the following command to run a factor analysis.
FA = factanal(motivations, nclust, rotation="varimax") 
print(FA) 

# 2biii What are the important features of each potential factor according to the 
# loadings? 
FA$loadings
# Factor #1: tech defensive, tech drive consistency
# Factor #2: bad_GM, highbill
# Factor #3: not enough hulling, not enough two capacity, difficulty to drive
# Factor #4: not enough stability, adjslow, hear others

# 2biv. Please name the factors according to the loadings of each motivations and 
# state your reasons. 
# Factor 1: Driving Experience
# Factor 2: Bad_GM
# Factor 3: Hauler
# Factor 4: Need Assistance

# 3a. Identify the high loading for each factor by determine their cut point p1, p2, p3, 
# and p4, respectively. 
# What is their cut point?
p1 = 0.40
p2 = 0.50
p3 = 0.30
p4 = 0.30

F1 = FA$loadings[,c(1)]*(abs(FA$loadings[,1])>=p1) 
F2 = FA$loadings[,c(2)]*(abs(FA$loadings[,2])>=p2) 
F3 = FA$loadings[,c(3)]*(abs(FA$loadings[,3])>=p3) 
F4 = FA$loadings[,c(4)]*(abs(FA$loadings[,4])>=p4) 
AdjFloading = cbind(F1, F2, F3, F4) 

# 3b. Create a matrix score that weighted each motivation by factor loading with cut 
# point in question a.
score = as.matrix(motivations) %*% AdjFloading

# 3c. Create a variable stdscore that standardize the score.
stdscore = matrix(apply(score, 2, mean), ncol=nclust,)
sdd =


# 4a. Create a variable cluster that clustering the respondents using the score and nclust 
# from question 1.
RNGkind(sample.kind = "Rounding")
set.seed(43)  
cluster = kmeans(stdscore, centers = nclust)

# 4b. How many respondents are there in each cluster? 
print(cluster)

# 4c. Interpret the cluster results and name each cluster. 
# Utilize the factors you named and then what they said for each by seeing a
# matrix of the structures

# 5a. Create a variable group using results in cluster to assigned each respondent to a 
# segment.not sure how to do
group = cluster$cluster
  
# 5b. Convert group into a set of dummy variables using the following command. 
  clust = matrix(0, ncol=nclust, nrow = nhh) 
for(i in 1:nhh){ 
  clust[i,group[i]]=1 
}

# 5c. Save clust to a .csv file with name Cluster.csv.
write.csv(clust, sep=",")

# Homework No. 6 - Caroline Eggett

#1a. 
set.seed(53) 
clustmean = read.csv("cluster mean.csv",sep=",",header = TRUE) 
rownames = clustmean[,1] 
clustmean = clustmean[,-1] 
rownames(clustmean) = rownames 

DriveExp = clustmean[,1]
Hauler = clustmean[,2]
Bad_GM = clustmean[,3]
Need_Assis = clustmean[,4]

#2a.
# Not sure how to do

#2b
# pWTP = matrix(NA, ncol =  , nrow =  ) 
# 
# for(c in 1:4){ 
#   for(var in 1:25){ 
#     partworth = clustmean[ , ] 
#     pcoeff = clustmean[ , ] 
#     pWTP[ , ] = (partworth/-pcoeff)*1000 
#   } 
# } 
# 
# colnames(pWTP) = c("Driv Exp","Hauler","Bad_GM","Need_Assis") 
# rownames(pWTP) = rownames[1:25] 
# pWTP

#3. This question explores the identification of target segment for your assigned brand. 
# 3a. Based on your analysis results from question 2b.) and a comparison of the pWTP 
# of brands across segments. 

# 3ai. Order the four segments in terms of their profitability of your assigned 
# brand. Which segment is the most profitable segment? 

# 3aii. Order the four segments in terms of their profitability of other brands, 
# respectively. Which segment is the most profitable segment for your 
# competitors, respectively? 

# 3aiii. For each segment, compare the profitability of your assigned brand and 
# the other brand. In which segment does your assigned brand have more 
# advantage for competition?  

# 3b. Based on your analysis results of Assignment#3, which segment is the target 
# segment of your assigned brand. 

# 3c. Combining your answer from a.) and b.), which segment should your assigned 
# brand target?

#4a.
# Base=rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25), # 
#            different brands, HC30/40, AWD, $25(,000) 
#            c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(rep(0,26))) 
# 
# #4b.
# Scen_Ford_ACC=rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  0,1,1,1,1, 25), # 
#                     different brands, HC30/40, AWD, $25(,000) 
#                     c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(rep(0,26)))

#4c. Create xb_base

#4d. Create xb_scen

#4e. Create the tWTP

#4f. Interpret tWTP

#5b. Create xb_ford_base

#5c. Create Mktshare_ford_base

#5d. Create xb_ford_scen

#5e.Create Mktshare_ford_scen

#5f. Calculate change

#5g. Interpret WTB.

#6 Create bundle.
Scen_Ford_bundle1= 
  rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  0,1,1,0,1,  0,1,1,1,1, 25),  
        c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(rep(0,26)) 
  )   

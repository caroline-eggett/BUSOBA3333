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

#2a.Use the following equation to calculate the pWTP of Adaptive cruise control.
# pWTP = BetaFeature / -BetaPrice * 1000
# Not sure how to do

#2b. Complete the following command to calculate the pWTP of all product features and segments.  
# (Hint: i) Create a saving space pWTP using matrix(); ii) Create a two levelssubroutines 
# using for(), where the first subroutine calculate across clusters and 
# the second subroutine calculate attribute for each cluster; iii) save the estimated 
# pWTP to the corresponding cell in pWTP) 
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

#4a.Copy the following command to create a variable Base that simulate a basic 
# condition market that assumes all brands offer an SUV with mpg 30/40, AWD, 
# all package features, and is priced at $25,000  
# Base=rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25), # 
#            different brands, HC30/40, AWD, $25(,000) 
#            c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#            c(rep(0,26))) 
# 
# #4b. Copy the following command to create a variable Scen_Ford_ACC that simulate a 
# comparison condition market for Ford Escape that assumes to be mpg 30/40, 
# AWD, all package features except Adaptive cruise control, and is priced at 
# $25,000. 
# Scen_Ford_ACC=rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  0,1,1,1,1, 25), # 
#                     different brands, HC30/40, AWD, $25(,000) 
#                     c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
#                     c(rep(0,26)))

#4c. Create a variable xb_base that calculate the attainable utility of basic condition 
# market (i.e., Base) using the following equation. 

#4d. Create a variable xb_scen that calculate the attainable utility of comparison 
# condition market (i.e., Scen_Ford_ACC) using the following equation. 

#4e.Use the following equation to calculate the tWTP of Adaptive cruise control for 
# Ford Escape.

#4f.  Please interpret the tWTP of the Adaptive cruise control for the Ford Escape to 
# guide their pricing strategy. 

#5b. Create a variable xb_ford_base that calculate the attainable utility of the Ford 
# Escape in the basic condition market condition (i.e., Base) using the following 
# equation. (Hint: use Base %*% Hauler to calculate the attainable utility for all 
# brands) 

#5c. Create a variable Mktshare_ford_base that calculate the market share of the Ford 
# Escape in the basic condition market condition using the following equation. 
# (Hint: use Base %*% Hauler to calculate the attainable utility for all brands) 

#5d. Create a variable xb_ford_scen that calculate the attainable utility of the Ford 
# Escape in the comparison condition market (i.e., Scen_Ford_ACC) using the 
# following equation. (Hint: use Scen_Ford_ACC  %*% Hauler to calculate the 
# attainable utility for all brands) 

#5e.Create a variable Mktshare_ford_scen that calculate the market share of the Ford 
# Escape in the comparison condition market using the following equation. (Hint: 
# use Scen_Ford_ACC  %*% Hauler to calculate the attainable utility for all 
# brands) 

#5f. Take the answers from question c.) and f.) and calculate the change of market 
# share for the Ford Escape if the Adaptive cruise control is not available for the 
# vehicle using the following equation. 

#5g. Please interpret the WTB of Adaptive cruise control for the Ford Escape to guide 
# their strategy.

#6 Create bundle.
Scen_Ford_bundle1= 
  rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  0,1,1,0,1,  0,1,1,1,1, 25),  
        c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(rep(0,26)) 
  )   

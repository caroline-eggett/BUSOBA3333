# Homework No. 6 - Caroline Eggett

#1a. 
set.seed(53) 
clustmean = read.csv("cluster mean.csv",sep=",",header = TRUE) 
rownames = clustmean[,1] 
clustmean = clustmean[,-1] 
rownames(clustmean) = rownames 

DrivExp = clustmean[,1]
Hauler = clustmean[,2]
Bad_GM = clustmean[,3]
Need_Assis = clustmean[,4]

#2a.Use the following equation to calculate the pWTP of Adaptive cruise control.
# pWTP = BetaFeature / -BetaPrice * 1000

# Drive Experience
(DrivExp[21]/-DrivExp[26])*1000

# Hauler
abs((clustmean[21,2]/-clustmean[26,2])*1000)

# Bad_GM
abs((clustmean[21,3]/-clustmean[26,3])*1000)

# Need_Assis
(clustmean[21,4]/-clustmean[26,4])*1000

#2b. Complete the following command to calculate the pWTP of all product features and segments.  
# (Hint: i) Create a saving space pWTP using matrix(); ii) Create a two levelssubroutines 
# using for(), where the first subroutine calculate across clusters and 
# the second subroutine calculate attribute for each cluster; iii) save the estimated 
# pWTP to the corresponding cell in pWTP) 
pWTP = matrix(NA, ncol = 4 , nrow =  25) 

for(c in 1:4){
  for(var in 1:25){
    partworth = clustmean[var, c]
    pcoeff = clustmean[nrow(clustmean), c]
    pWTP[var, c] = (partworth/-pcoeff)*1000
  }
}

colnames(pWTP) = c("Driv Exp","Hauler","Bad_GM","Need_Assis")
rownames(pWTP) = rownames[1:25]
pWTP

#3. This question explores the identification of target segment for your assigned brand. 
# 3a. Based on your analysis results from question 2b.) and a comparison of the pWTP 
# of brands across segments. 

# 3ai. Order the four segments in terms of their profitability of your assigned 
# brand. Which segment is the most profitable segment? 

# My Brand was the Ford Escape.  The ranking is as follows:
# 1. Bad_GM
# 2. Hauler
# 3. Need_Assis
# 4. Driv Exp

# 3aii. Order the four segments in terms of their profitability of other brands, 
# respectively. Which segment is the most profitable segment for your 
# competitors, respectively? 

# Driv Exp: Toyota.RAV4 has the highest profitability.
# Hauler: Honda CR.V has the highest profitability.
# Bad_GM: Toyota RAV4 has the highest profitability.
# Need_Assis: Honda CR.V has the highest profitability.

# 3aiii. For each segment, compare the profitability of your assigned brand and 
# the other brand. In which segment does your assigned brand have more 
# advantage for competition?  

# Based on the other brand's profitabilities, the Ford Escape seems to be #3 in
# Hauler and Bad_GM, therefore I think Ford will have a higher advantage in these
# two segments.

# 3b. Based on your analysis results of Assignment#3, which segment is the target 
# segment of your assigned brand. 

# I think the Hauler segment is the garget segment from Assignment #3.
# If you look at Ford's slogan, "Shaking Up the Segment with 
# Versatility and Capability," it reads that Ford is allow their customers to be
# able to take their vehicles off-road, haul more things, make it a vehicle that
# can do basically anything.

# 3c. Combining your answer from a.) and b.), which segment should your assigned 
# brand target?

# I think the Hauler segment is the target segment for a Ford Escape since it
# is in the top 3 by roughly ~$1,000 more than the next brand below them.  
# Furthermore, I the price difference from the top brand (Honda) and Ford is roughly
# ~$1,5000.

#4a.Copy the following command to create a variable Base that simulate a basic 
# condition market that assumes all brands offer an SUV with mpg 30/40, AWD,
# all package features, and is priced at $25,000
Base=rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
           # different brands, HC30/40, AWD, $25(,000)
           c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
           c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
           c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
           c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
           c(rep(0,26)))

# #4b. Copy the following command to create a variable Scen_Ford_ACC that simulate a 
# comparison condition market for Ford Escape that assumes to be mpg 30/40, 
# AWD, all package features except Adaptive cruise control, and is priced at 
# $25,000. 
Scen_Ford_ACC=rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
                    c(0,1,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  0,1,1,1,1, 25),
                    # different brands, HC30/40, AWD, $25(,000)
                    c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
                    c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
                    c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),
                    c(rep(0,26)))

#4c. Create a variable xb_base that calculate the attainable utility of basic condition 
# market (i.e., Base) using the following equation. 
xb_base = sum(exp(Base %*% Hauler))

#4d. Create a variable xb_scen that calculate the attainable utility of comparison 
# condition market (i.e., Scen_Ford_ACC) using the following equation. 
xb_scen = sum(exp(Scen_Ford_ACC %*% Hauler))

#4e.Use the following equation to calculate the tWTP of Adaptive cruise control for 
# Ford Escape.
tWTP = (log(xb_base)-log(xb_scen))/(-Hauler[26])*1000
print(tWTP)

#4f.  Please interpret the tWTP of the Adaptive cruise control for the Ford Escape to 
# guide their pricing strategy. 

# The absolute value of this number ($232.54) is the amount that someone is willing
# to pay for the additional of adaptive cruise control for a Ford Escape.

#5b. Create a variable xb_ford_base that calculate the attainable utility of the Ford 
# Escape in the basic condition market condition (i.e., Base) using the following 
# equation. (Hint: use Base %*% Hauler to calculate the attainable utility for all 
# brands) 
xb_ford_base = Base %*% Hauler

#5c. Create a variable Mktshare_ford_base that calculate the market share of the Ford 
# Escape in the basic condition market condition using the following equation. 
# (Hint: use Base %*% Hauler to calculate the attainable utility for all brands) 
Mktshare_ford_base = exp(xb_ford_base[2])/sum(exp(xb_ford_base))

#5d. Create a variable xb_ford_scen that calculate the attainable utility of the Ford 
# Escape in the comparison condition market (i.e., Scen_Ford_ACC) using the 
# following equation. (Hint: use Scen_Ford_ACC  %*% Hauler to calculate the 
# attainable utility for all brands) 
xb_ford_scen = Scen_Ford_ACC %*% Hauler

#5e.Create a variable Mktshare_ford_scen that calculate the market share of the Ford 
# Escape in the comparison condition market using the following equation. (Hint: 
# use Scen_Ford_ACC  %*% Hauler to calculate the attainable utility for all 
# brands) 
Mktshare_ford_scen = exp(xb_ford_scen[2])/sum(exp(xb_ford_scen))

#5f. Take the answers from question c.) and f.) and calculate the change of market 
# share for the Ford Escape if the Adaptive cruise control is not available for the 
# vehicle using the following equation. 
Mktshare_ford_scen - Mktshare_ford_base # 11.02347 change

#5g. Please interpret the WTB of Adaptive cruise control for the Ford Escape to guide 
# their strategy.

# It appears that by having the ability for a customer to add adaptive cruise
# control, it creates a 11.02% gain in market share.  With that, Ford should price
# their product with adaptive cruise control around $232 (this is the price that
# customers are willing to pay for this feature).

#6 Create bundle.
# Ford without Power Sunroof + Dual Zone Temperature Control + Premium Stereo
# and Carpeted Floor Mats
# (mimic Ford Premium Luxury Items) 

Scen_Ford_bundle1= 
  rbind(c(1,0,0,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,1,0,0,0, 0,0,0,1, 0, 0,0,0,0,0,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,0,1,0,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,0,0,1,0, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(0,0,0,0,1, 0,0,0,1, 0, 1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1, 25),  
        c(rep(0,26)) 
  ) 

xb_scen_bundle = sum(exp(Scen_Ford_bundle1 %*% Hauler))

tWTP = (log(xb_base)-log(xb_scen_bundle))/(-Hauler[26])*1000
print(tWTP)
# The tWTP is $275.52

xb_ford_scen_bundle = Scen_Ford_bundle1 %*% Hauler
Mktshare_ford_scen_bundle = exp(xb_ford_scen[2])/sum(exp(xb_ford_scen_bundle))
Mktshare_ford_scen_bundle - Mktshare_ford_base
# The increase in market share is 21.08%

# This means by pricing the premium luxury Ford Escape package at $275.12, the
# market share will increase by 21.08% (since customers will pay this value for
# this enhancement in features for a Ford Escape).
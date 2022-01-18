# Homework No. 1 - Caroline

#1a
a <- 1
b <- 3

a + b
a - b
a * b
a / b

#1b

x <- c(1, 2, 3, 4, 5, 6)

x + 3
x - 3
x * 3
x / 3
sum(x)


#1c

y <- c(1, 3, 5, 7, 9, 11)

x + y
x - y
x * y
x / y
(x + 3) / y
sum(x * y) #not sure if this is right, check with students.

#1d

z <- sample(x, size = 6, replace = TRUE)

x + z
x - z
x * z
x / z

#1e

w <- rnorm(6, mean = 0, sd = 1)
lm(formula= w ~ x + z)

#1f
vector1 <- seq(1, 100, 1)

#1g
vector2 <- rep(1, 100)

#2a
is.matrix(x) #FALSE
is.matrix(y) #FALSE
is.matrix(z) #FALSE
is.matrix(w) #FALSE

#2b
x2 <- t(x)

#2c
y2 <- t(y)

#2d
x2 + y2
x2 - y2
x2 * y2
x2 / y2
#x2%*%y2 #error: non-conformable arguments
t(y2)
x2%*%t(y2) #this gives back 161

#Yes, it is the same.

#2e

matrix(x, nrow = 2, ncol = 3, byrow = TRUE)
matrix(x, nrow = 2, ncol = 3, byrow = FALSE)
# the values in the matrices are different, (NEED TO ELABORATE MORE)

#3a
h <- rnorm(20, mean = 0, sd = 1)
hist(h, main = "Norm~(0,1) of Size 20", xlab = "Standard Normal Distribution")
matplot(h, type = "l")
v <- h
plot(h, v, main = "Scatter Plot of H and V", xlab = "H Values",
     ylab = "V Values")

#4
setwd("/Users/carol/OneDrive - The Ohio State University/2022 Spring/BUSOBA 3333/Ford Data")

#4a
data <- read.csv("Ford_Data.csv", sep=",", header = TRUE)

#4b
varname <- colnames(data)

#4c
str(data) # 547 rows and 248 columns
dim(data)
# str(data) gives each variable with its respectable values
# while dim(data) gives just the row by column (so the dimensions)

data[1,] # gives row one, all columns
data[,6] # gives all rows, column #6 values
data[1,6] # gives row one, column $6 value

#4d
respt <- data$ILength
time <- data[,grepl("Time", names(data))]
budget <- data[,c("S5_Price", "Q1")]
mean(respt) # 1535.201
mean(budget$S5_Price) # 30,966.61
mean(budget$Q1) # 41,428.88
summary(budget)
apply(budget, 2, FUN = mean)

#5

# creating a variable with a matrix of the Ford data
data = data.matrix(read.csv("Ford_Data.csv",sep=",",header = TRUE)) 

# getting a subset of the data rows x columns
data = data[,-c(1,3)] # delete a certain number of columns

# getting the column names into a variable
varname = colnames(data) 

# reading the Ford Data CSV, storing in a variable
data = read.csv("Ford_Data.csv") 

# getting a subset of the data
data = data[,-c(1,3)] # delete a certain number of columns

#getting the column names in the Ford Data and putting them into the variable
#varname
colnames(data) = varname 

# Create convience variables 
nhh = nrow(data) 

# Begin creating individual data list 
# create empty list
Data.Main = NULL
# create for loop
for(hh in 1:nhh){ 
  # select respondent's choice 
  y.1   = matrix(as.numeric(data[data[,1]==hh , grepl("Q4_", names( data ) 
  ) ]),  ncol = 1) 
  
  # select specific variables 
  pattern_Need = c("Q2_","Q3_")
  # assigning Need the value in hh row index that is AND in Q2_ and Q3_
  Need = data[data[,1]==hh, grepl(paste(pattern_Need, collapse="|") , 
                                  names( data ) ) ]
  # starting a matrix
  Need = matrix(as.numeric(Need[,-c(19,33)]),nrow=1) 
  
  # select specific variables
  pattern_Demo = 
    c("D4","D5","D6","D7","Male","Age_","Ethnicity_","Income_") 
  # assigning Demo the value in hh row index that is AND in columns in Demo
  Demo = matrix(as.numeric(data[data[,1]==hh, grepl(paste(pattern_Demo, 
                                                          collapse="|") , names( data ) ) ]),nrow=1) 
  # select specific variables
  pattern_Budget = c("S5_","Q1") 
  # assigning Need the value in hh row index that is AND in S5_ and Q1
  Budget = data[data[,1]==hh, grepl(paste(pattern_Budget, collapse="|") , 
                                    names( data ) ) ] 
  
  # creating a list with each of the above variables
  Z = list(Need = Need, Budget = Budget, Demo = Demo) 
  
  # assigning an additional column with respondents values to the list
  Data.Main[[hh]] = list( y = as.numeric(y.1)) 
}

#5c
Data.Main[[1]] #this represents the Need, Demo, and Budget columns for row index #1
Data.Main[[1]]$Need #this is NULL
Data.Main[[1]][1] #this is says the same this as the first one.
# These represent the values of Need, Budget, and Demo for all 547 rows in the
# original Ford Data.

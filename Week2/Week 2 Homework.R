#      Course:          IST687
#      Name:            Jeremy Toepp
#      Homework:        Week 2
#      Due Date:        4/12/2020
#      Date Submitted:  4/9/2020
#

# ---------- HW2: Intro -----------
myCars <- mtcars #rename the mtcars dataset to preserve changes

## Step 1: What is the hp (hp stands for "horse power")
# 1) What is the highest hp?
maxHp <- max(myCars$hp) #assign max hp to a variable
cat("The max horsepower is:", maxHp) #return the statement along with the variable, like printing hello world

# 2) Which car has the highest hp?
maxHpCar <- row.names(myCars[which.max(myCars$hp),]) #find the row name of the row containing the car with the max hp
cat("The car with the maximum horsepower is:", maxHpCar) #return the variable

## Step 2: Explore mpg (mpg stands for "miles per gallon")
# 3) What is the highest mpg?
maxMpg <- max(myCars$mpg) #assign the max mpg to a variable
cat("The max MPG is:", maxMpg) #print the statement and variable contents

# 4) Which car has the highest mpg?
maxMpgCar <- row.names(myCars[which.max(myCars$mpg),]) #find the row name of the row containing the car with the max hp
cat("The car with the maximum miles per gallon is:", maxMpgCar) #return the variable

# 5) Create a sorted dataframe, based on mpg
highMPGcars <- data.frame(myCars[order(-myCars$mpg),]) #create a dataframe sorted by highest mpg
highMPGcars #print the dataframe

a## Step 3: Which car has the "best" combination of mpg and hp?
# 6) What logic did you use?
#create a ratio of mpg to hp by dividing and add that as a new column to myCars
myCars$efficiency <- myCars$mpg/amyCars$hp 
max(myCars$efficiency) #return the maximum ratio

# 7) Which car?
maxRatio <- row.names(myCars[which.max(myCars$efficiency),]) #find the row name of the row that contains the highest ratio
cat("The most efficient car is:", maxRatio) #return the variable

## Step 4: Which car has "best" car combination of mpg and hp, where mpg and hp must be given equal weight?
# use scale to properly compare the impact of mpg vs hp
myCars$scaledEfficiency <- scale(myCars$mpg) + scale(myCars$hp)
maxScaledEfficiency <- row.names(myCars[which.max(myCars$scaledEfficiency),]) #find the row name of the row that contains the highest ratio
cat("The most efficient car using scale() is:", maxScaledEfficiency) #return the variable

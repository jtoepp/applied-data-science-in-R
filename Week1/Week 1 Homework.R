#      Course:          IST687
#      Name:            Jeremy Toepp
#      Homework:        Week 1
#      Due Date:        4/5/2020
#      Date Submitted:  4/6/2020
#

# ---------- HW1: Intro -----------
height <- c(59,60,61,58,67,72,70) #create vector and fill with data
weight <- c(150,140,180,220,160,140,130) # create vector and fill
a <- 150 #assign 150 to variable 'a' for later use

## Step 1: Calculating Means
# 1) Compute, using R, the average height (called mean in R)
meanHeight <- mean(height)  #assign the mean of height
meanHeight   #print the variable

# 2) Compute, using R, the average weight (called mean in R)
meanWeight <- mean(weight)  #assign the mean of weight
meanWeight  #print the variable

# 3) Calculate the length of the vector 'height' and 'weight'
lengthHeight <- length(height) #assign the length of height
lengthHeight  #print the variable
lengthWeight <- length(weight) #assign the length of weight
lengthWeight  #print the variable

# 4) Calculate the sum of the heights
sumHeight <- sum(height) #assign the sum of height
sumHeight  #print the variable

# 5) Compute the average of both height and weight, by dividing the sum 
# (of the height or the width, as appropriate), by the length of the
# vector. How does this compare to the 'mean' function?
averageHeight <- sumHeight/lengthHeight #assign average height using vector division
averageHeight #print the variable
sumWeight <- sum(weight) #assign the sum of weight to prep for finding the average
averageWeight <- sumWeight/lengthWeight
averageWeight #print the variable
# as for how this compares to the mean() function--both result in the same answer


## Step 2: Using max/min functions
# 6) Compute the max height, store the result in 'maxH'
maxH <- max(height) #assign the max of height to a variable
maxH #print the variable

# 7) Compute the min weight, store the results in 'minW'
minW <- min(weight) #assign the min of weight to a variable
minW #print the variable


## Step 3: Vector Math
# 8) Create a new vector, which is the weight + 5 (every person gained 5 pounds)
newWeight <- weight + 5 #use vector math to add 5 to each observation
newWeight #print the variable

# 9) Compute the weight/height for each person, using the new weight just created
whRatio <- newWeight/height #calculate ratio of weight to height using new weight
whRatio #print the variable


## Step 4: Using Conditional if statements
# 10) Write the R code to test if max height is greater than 60 (output "yes" or "no")
if (maxH > 60) "yes" else "no" #test if maxH (72) is greater than 60 >>> (yes)

# 11) Write the R code to if min weight is greater than the variable 'a' (output "yes" or "no")
if (minW > a) "yes" else "no" #test if minW (130) is greater than a (150) >>> (no)

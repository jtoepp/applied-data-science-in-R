#      Course:          IST687
#      Name:            Jeremy Toepp
#      Homework:        Week 4
#      Due Date:        4/26/2020
#      Date Submitted:  4/26/2020
#

# ---------- HW4: Intro -----------
# set working directory to save the plots later
setwd("/Documents/College/Masters/SyracuseUniversity/IST 687 - Applied Data Science/Homework/Week 4")

# Step 1: Write a summarizing function to understand the distribution of a vector
# 1.	The function, call it 'printVecInfo' should take a vector as input
# 2.	The function should print the following information: Mean, Median, Min & max, Standard 
#     deviation, Quantiles (at 0.05 and 0.95), Skewness
# 3.	Test the function with a vector that has (1,2,3,4,5,6,7,8,9,10,50).
# create a function that takes a vector as an input and outputs several descriptive statistics
# include moments library to access skewness
library("moments")
printVecInfo <- function(vector){
  cat("printVecInfo results:","\n")
  cat("mean:",mean(vector),"\n")
  cat("median:",median(vector),"\n")
  cat("min:",min(vector),"\n")
  cat("max:",max(vector),"\n")
  cat("standard deviation:",sd(vector),"\n")
  quant <- quantile(vector,c(0.05,.95))
  cat("5th to 95th percentile:",quant[1],"--",quant[2],"\n")
  cat("skewness:",skewness(vector),"\n")
}

# create a vector to test the function with
testVector <- c(1,2,3,4,5,6,7,8,9,10,50)
# test the function
printVecInfo(testVector)

# Step 2: Creating Samples in a Jar
# 4.	Create a variable 'jar' that has 50 red and 50 blue marbles
# create the variable with half red and blue marbles
# the strings for later replication
red <- "red"
blue <- "blue"

# create the two buckets before the jar
v.red <- replicate(50,red)
v.blue <- replicate(50,blue)

# concatenate into a jar and verify the length
jar <- c(v.red,v.blue)
lengthJar <- length(jar)
cat("There are", lengthJar, "marbles in the jar.")

# 5.	Confirm there are 50 reds by summing the samples that are red
# check to see how many are red
redMarbles <- length(jar[jar=='red'])
cat("There are", redMarbles, "red marbles in the jar.")
## also could use length(grep("red",jar))

# 6.	Sample 10 'marbles' (really strings) from the jar. How many are red? What was the percentage
#     of red marbles?
# sample 10 marbles from the jar
sample10 <- sample(jar,10,replace=TRUE)
cat("10 sample draws from the jar:", sample10)
# test how many are red
sample10Red <- length(sample10[sample10==red])
cat("Number of red marbles:",sample10Red)
# test percentage of red
sample10RedPerc <- length(sample10[sample10==red])/length(sample10)
cat("Percent red marbles: ",sample10RedPerc*100, "%")
  
# 7.	Do the sampling 20 times, using the 'replicate' command. Use your printVecInfo to see 
#     information of the samples. Also generate a histogram of the samples.
# create a function to replicate samples of different sizes
myTest <- function(jar,num){
  mySample <- sample(jar,num,replace=TRUE)
  len <- length(mySample[mySample==red])
  pct <- len/num*100
  return(pct)
}

# replicate a sample of 10, 20 times
test1 <- replicate(20,myTest(jar,10))

# utilize printVecInfo to see info about the variable
printVecInfo(test1)

# plot the tests as a customized histogram
hist(
  test1,
  main="Histogram for Red Marbles: 20 replications of 10 samples", 
  xlab="Percent of Red Marbles", 
  border="cyan", 
  col="red"
     )
# save the histogram to file
dev.copy(png,"20-10 histogram.png")

# close the PNG device now that we are all done
dev.off()

# 8.	Repeat #7, but this time, sample the jar 100 times. Use your printVecInfo to see information
#     of the samples. Also generate a histogram of the samples.
# replicate a sample of 100, 20 times
test2 <- replicate(20,myTest(jar,100))

# utilize printVecInfo to see info about the variable
printVecInfo(test2)

# plot the tests as a customized histogram
hist(
  test2,
  main="Histogram for Red Marbles: 20 replications of 100 samples", 
  xlab="Percent of Red Marbles", 
  border="cyan", 
  col="red"
)

# save the histogram to file
dev.copy(png,"20-100 histogram.png")

# close the PNG device now that we are all done
dev.off()

# 9.	Repeat #8, but this time, replicate the sampling 100 times. Use your printVecInfo to see 
#     information of the samples. Also generate a histogram of the samples.
# replicate a sample of 100, 100 times
test3 <- replicate(100,myTest(jar,100))

# utilize printVecInfo to see info about the variable
printVecInfo(test3)

# plot the tests as a customized histogram
hist(
  test3,
  main="Histogram for Red Marbles: 100 replications of 100 samples", 
  xlab="Percent of Red Marbles", 
  border="cyan", 
  col="red"
)

# save the histogram to file
dev.copy(png,"100-100 histogram.png")

# close the PNG device now that we are all done
dev.off()


# Step 3: Explore the airquality dataset
# 10.	Store the 'airquality' dataset into a temporary variable
myAirQuality <- airquality

# 11.	Clean the dataset (i.e. remove the NAs)
noNaN_myAirQuality <- na.omit(myAirQuality) # use this one
noNaN_myAirQuality[is.na(noNaN_myAirQuality)] <- 0
noNaN_myAirQuality$Ozone[is.na(noNaN_myAirQuality$Ozone)] <- mean(noNaN_myAirQuality$Ozone,na.rm=TRUE)

# 12.	Explore Ozone, Wind and Temp by doing a 'printVecInfo' on each as well as generating a 
#     histogram for each
# let's make things more interesting with a custom color package
library("grDevices")

# get info about the ozone column
printVecInfo(noNaN_myAirQuality$Ozone)
# customize the color ramp
pal <- colorRampPalette(c("darkgreen", "lightgreen"))
# plot the customized histogram
hist(
  noNaN_myAirQuality$Ozone,
  main="Air Quality Dataset - Ozone", 
  xlab="Ozone Level - parts per billion", 
  border="cyan", 
  col=pal(20),
  breaks=20
)
# save the histogram to file
dev.copy(png,"ozone histogram.png")
# close the PNG device now that we are all done
dev.off()


# get info about the wind column
printVecInfo(noNaN_myAirQuality$Wind)
# customize the color ramp
pal <- colorRampPalette(c("cyan2", "purple4"))
# plot the customized histogram
hist(
  noNaN_myAirQuality$Wind,
  main="Air Quality Dataset - Wind", 
  xlab="Wind Speed - miles per hour", 
  border="lightgray", 
  col=pal(19),
  breaks=20
)
# save the histogram to file
dev.copy(png,"wind histogram.png")
# close the PNG device now that we are all done
dev.off()


# get info about the temperature column
printVecInfo(noNaN_myAirQuality$Temp)
# set a custom palette for a gradient color as temperature increases
pal <- colorRampPalette(c("blue", "red", "darkred"))
# plot the customized histogram
hist(
  noNaN_myAirQuality$Temp,
  main="Air Quality Dataset - Temperature", 
  xlab="Temperature (F)", 
  border="lightgray", 
  col=pal(21),
  breaks=20
)
# save the histogram to file
dev.copy(png,"temp histogram.png")
# close the PNG device now that we are all done
dev.off()


# get info about the solar radiation column
printVecInfo(noNaN_myAirQuality$Solar.R)
# set a custom palette for a gradient color as temperature increases
pal <- colorRampPalette(c("yellow", "orange4"))
# plot the customized histogram
hist(
  noNaN_myAirQuality$Solar.R,
  main="Air Quality Dataset - Solar Radiation", 
  xlab="Solar Radiance - Langleys (4000-7700 Angstroms)", 
  border="lightgray", 
  col=pal(21),
  breaks=20
)
# save the histogram to file
dev.copy(png,"solrad histogram.png")
# close the PNG device now that we are all done
dev.off()


# just playing around--here is a second version of printVecInfo that also removes NAs
noNA_printVecInfo <- function(vector){
  cat("printVecInfo results:","\n")
  cat("mean:",mean(vector,rm.na=TRUE),"\n")
  cat("median:",median(vector,rm.na=TRUE),"\n")
  cat("min:",min(vector,rm.na=TRUE),"\n")
  cat("max:",max(vector,rm.na=TRUE),"\n")
  cat("standard deviation:",sd(vector,rm.na=TRUE),"\n")
  quant <- quantile(vector,c(0.05,.95),rm.na=TRUE)
  cat("5th to 95th percentile:",quant[1],"--",quant[2],"\n")
  cat("skewness:",skewness(vector,rm.na=TRUE),"\n")
}
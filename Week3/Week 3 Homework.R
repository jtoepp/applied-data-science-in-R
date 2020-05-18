#      Course:          IST687
#      Name:            Jeremy Toepp
#      Homework:        Week 3
#      Due Date:        4/19/2020
#      Date Submitted:  4/19/2020
#

# ---------- HW3: Intro -----------
## Step 1: Create a function (named readStates) to read a CSV file into R
## Step 2: Clean the dataframe
# create a function to read a url, then proceed to cleaning that data
readStates <- function()
{
  readUrl <- ("http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv")
  urlToStates <- read.csv(url(readUrl)) # pass the url to a variable then read it into a variable
  urlToStates <- urlToStates[-1:-8,] # remove the first 8 rows
  urlToStates <- urlToStates[,1:5] # remove the remaining columns
  urlToStates <- urlToStates[-52:-58,] # remove the trailing rows (originally 60:66)
  names(urlToStates)[1] <- "stateName" # rename the massive column name for column 1
  names(urlToStates)[2] <- "base2010" # rename the column to match base 2010 data
  names(urlToStates)[3] <- "base2011" # rename the column to match base 2011 data
  names(urlToStates)[4] <- "jul2010" # rename the column to match jul 2010 data
  names(urlToStates)[5] <- "jul2011" # rename the column to match jul 2011 data
  urlToStates$stateName <- substring(urlToStates$stateName,2) # select all but first character of each string
  # at the same time, convert the fields to numeric and remove all commas
  urlToStates$base2010 <- as.numeric(gsub(",","",urlToStates$base2010))
  urlToStates$base2011 <- as.numeric(gsub(",","",urlToStates$base2011))
  urlToStates$jul2010 <- as.numeric(gsub(",","",urlToStates$jul2010))
  urlToStates$jul2011 <- as.numeric(gsub(",","",urlToStates$jul2011))
}

# run the function
readStates()

## Step 3: Store and Explore the dataset
# 6. Store the dataset into a dataframe, called dfStates.
# store the dataset from above in a dataframe
dfStates <- data.frame(urlToStates)


# 7. Test your dataframe by calculating the mean for the July2011 data, by doing: mean(dfStates$Jul2011) testing
# dataframe by calulating the mean for July 2011 data
july2011Mean <- mean(dfStates$jul2011)
# return the variable
cat("The mean population for July 2011 is:", july2011Mean)


## Step 4: Find the state with the Highest Population
# 8. Based on the July2011 data, what is the population of the state with the highest population? What is the
#    name of that state?
# find the highest population of any state
july2011MaxPop <- max(dfStates$jul2011)
# return the variable
cat("The highest population of any state for July 2011 is:", july2011MaxPop)

# find the name of the state with the highest population
july2011MaxPopState <- dfStates[which.max(dfStates$jul2011),1]
# return the variable
cat("The highest populated state for July 2011 is:", july2011MaxPopState)

# 9. Sort the data, in increasing order, based on the July2011 data.
# sort the dataframe by july 2011 population
dfStatesSort <- data.frame(dfStates[order(dfStates$jul2011),])
# return the dataframe
dfStatesSort

## Step 5: Explore the distribution of the states
# 10. Write a function that takes two parameters. The first is a vector and the second is a number.
# 11. The function will return the percentage of the elements within the vector that is less than the same (i.e.
#     the cumulative distribution below the value provided).
# 12. For example, if the vector had 5 elements (1,2,3,4,5), with 2 being the number passed into the function, 
#     the function would return 0.2 (since 20% of the numbers were below 2).
# create a function that calculates cumulative distribution
cumulativeDist <- function(stateVector, threshold)
{
  # initialize our count variable
  count = 0
  # run through the vector one value at a time
  for (i in stateVector)
    {
      # if the number is below the threshold, increment the count variable by 1
      if (i < threshold)
        count <- count + 1
    }
  # take the total result and divide it by the number of observations in the vector
  return(count/length(stateVector))
}

# test the function against the example by replicating the example
exampleVector <- c(1,2,3,4,5)
cumulativeDist(exampleVector, 2)
# it works!

# 13. Test the function with the vector 'dfStates$Jul2011Num', and the mean of dfStates$Jul2011Num'.
# test the function and pass it to a variable
cumulativeDistJul2011 <- cumulativeDist(dfStates$jul2011, mean(dfStates$jul2011))
# return the variable
cat("The cumulative distribution of population below the mean population for July 2011 is:", cumulativeDistJul2011)
# this means that 66.67% of states have a population below the mean population for the country





#      Course:          IST687
#      Name:            Jeremy Toepp
#      Homework:        Week 5
#      Due Date:        5/03/2020
#      Date Submitted:  5/03/2020
#

# ---------- HW5: Intro -----------
# set working directory for the current project
setwd("/Documents/College/Masters/SyracuseUniversity/IST 687 - Applied Data Science/Homework/Week 5")

# include the following libraries
library(RCurl)
library(sqldf)
library(jsonlite)
library(stringr)


## Step 1:  Load the data
# read in the JSON data from the following link
link <- "http://opendata.maryland.gov/resource/pdvh-tf2u.json"

# pass the data to a variable while parsing the JSON format
maryland <- na.omit(fromJSON(txt=link))


## Step 2:  Clean the data
# take a look at the current format
head(maryland,3)

# data already matches the format in the homework instructions
# remove leading and trailing white space
# results in warning message: In stri_trim_both(string) : argument is not an atomic vector; coercing
# trimMaryland <- str_trim(maryland, side=("both"))


## Step 3:  Understand the data using SQL (via SQLF)
# use sql to find the number of accidents that occur on a Sunday
# instead of LIKE could have used TRIM(day_of_week)
sundayAccidents <- sqldf("SELECT COUNT(case_number) FROM maryland WHERE day_of_week LIKE '%unday%';")
# return the results
cat("There were", toString(sundayAccidents),"accidents that occurred on a Sunday.")

# use sql to determine distinct values in injuries
sqldf("SELECT DISTINCT injury FROM maryland;")

# use sql to determine how many accidents had injuries
injuryAccidents <- sqldf("SELECT COUNT(injury) FROM maryland WHERE injury = 'YES';")
# return the results
cat("There were", toString(injuryAccidents), "accidents that involved injuries.")

# use sql to determine number of accidents per day using a CASE statement to order by day of week
injuryPerDay <- sqldf("
SELECT day_of_week AS 'Day of the Week', COUNT(injury) AS 'Number of Injuries'
FROM maryland 
WHERE injury = 'YES'
GROUP BY day_of_week 
ORDER BY
  CASE
    WHEN day_of_week LIKE '%unday%' THEN 1
    WHEN day_of_week LIKE '%onday%' THEN 2
    WHEN day_of_week LIKE '%uesday%' THEN 3
    WHEN day_of_week LIKE '%ednesday%' THEN 4
    WHEN day_of_week LIKE '%hursday%' THEN 5
    WHEN day_of_week LIKE '%riday%' THEN 6
    WHEN day_of_week LIKE '%aturday%' THEN 7
  END ASC;")
# return the results as the table that was created above
injuryPerDay


## Step 4:  Understand the data using tapply
# attach the dataset for easier referencing of the columns
attach(maryland)

# use tapply to find how many accidents occurred on a Sunday
tapply(injury,day_of_week,length)

# use tapply to find how many accidents had injuries
tapply(case_number,injury,length)

# use tapply to list the injuries by day
tapply(injury,list(injury,day_of_week),length)


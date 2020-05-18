#      Course:          IST687
#      Name:            Jeremy Toepp
#      Homework:        Week 7
#      Due Date:        5/17/2020
#      Date Submitted:  5//2020
#

# ---------- HW7: Intro -----------
# set working directory for the current project
setwd("/Documents/College/Masters/SyracuseUniversity/IST 687 - Applied Data Science/Homework/Week 7")

# handling google api key requires the ggmap version from Github rather than Cran
if (!requireNamespace("devtools")) install.packages("devtools")
if (!requireNamespace("ggmap")) install_github("dkahle/ggmap")
library(ggmap)

## Step 1:  Load the data
# pre-cursor functions:  readCensus, Numberize 
# read in the census data set
readCensus <- function() {
  urlToRead <-"http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  #read the data from the web
  testFrame <- read.csv(url(urlToRead))
  #remove the first 8 rows (header information)
  testFrame <- testFrame[-1:-8,]
  #only keep the first 5 columns
  testFrame <- testFrame[,1:5]
  #rename the first column
  testFrame$stateName <- testFrame[,1]
  testFrame <- testFrame[,-1]
  #remove the last rows (tail info)
  testFrame <- testFrame[-52:-58,]
  #remove the leading period from the state name
  testFrame$stateName <- gsub("\\.","", testFrame$stateName)
  #convert the columns to actual numbers and rename columns
  testFrame$april10census <- Numberize(testFrame$X)
  testFrame$april10base <- Numberize(testFrame$X.1)
  testFrame$july10pop <- Numberize(testFrame$X.2)
  testFrame$july11pop <- Numberize(testFrame$X.3)
  testFrame <- testFrame[,-1:-4]
  #remove the old rownames, which are now confusing
  rownames(testFrame) <- NULL
  return(testFrame)
}

# custom function to prep a vector and convert to a number
Numberize <- function(inputVector) {
  # Get rid of commas
  inputVector <- gsub(",","", inputVector)
  # Get rid of spaces
  inputVector <- gsub(" ","", inputVector)
  return(as.numeric(inputVector))
}

# Packages: maps, zipcode, mapproj, ggmap, ggplot2, gdata
# specify the packages of interest
packages = c("maps",
             "zipcode",
             "mapproj",
             "ggplot2",
             "gdata",
             "dplyr",
             "readxl",
             "tmaptools")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# verify they are loaded
search()

# state.name  - R data set
str(state.name)
head(state.name)

# store in a new dataset to preserve the original
stateNamesdf <- data.frame(state.name, stringsAsFactors=FALSE)
stateNamesdf$state <- tolower(stateNamesdf$state.name)
head(stateNamesdf)

# remove original column
stateNamesdf <- stateNamesdf %>%
  select(-state.name)

# map the data to a lat lon bounding areas using maps package
us <- map_data("state")
# take a look
str(us)
head(us)

# store the variable using the custom function
dfStates <- readCensus()
# take a look
str(dfStates)
head(dfStates)
# change to lower case
dfStates$stateName <- tolower(dfStates$stateName)
# remove original column
dfStates <- dfStates %>%
  rename(state = stateName)

# assign zip codes income data to a variable
medianZip <- read_excel("./Data/MedianZIP.xlsx")
str(medianZip)
head(medianZip)

# clean up the data
# delete the original header
medianZip <- medianZip[-1,]
# change column names
colnames(medianZip) <- c("zip", "Median", "Mean", "Population")
# use gsub to remove commas
medianZip$Median <- gsub(",", "", medianZip$Median)
medianZip$Mean <- gsub(",", "", medianZip$Mean)
medianZip$Population <- gsub(",","",medianZip$Population)
# take a look
head(medianZip)

# load zipcode dataset
data(zipcode)
# take a look
str(zipcode)
head(zipcode)

# clean.zipcodes https://www.rdocumentation.org/packages/zipcode/versions/1.0/topics/clean.zipcodes
medianZip$zip <- clean.zipcodes(medianZip$zip)
head(medianZip)
head(zipcode)

# merge(mydata, zipcode, by="zip") into a new df
dfMedianZip <- merge(medianZip, zipcode, by="zip")
str(dfMedianZip)
head(dfMedianZip)

# convert to numeric
dfMedianZip$Median<-as.numeric(dfMedianZip$Median)
dfMedianZip$Population<-as.numeric(dfMedianZip$Population)
# verify
str(dfMedianZip)

# calc mean of median by state
income <- tapply(dfMedianZip$Median, dfMedianZip$state, mean) 
str(income)
head(income)

# place rownames from income into state variable
state <- rownames(income)  
head(state)

# mean Median Income by State
# create a df with state variable & income variable
medianIncome <- data.frame(state, income)
str(medianIncome)
head(medianIncome)

# sum up population for each state
pop <- tapply(dfMedianZip$Population, dfMedianZip$state, sum ) 
str(pop)
head(pop)
# same content as earlier
state <- rownames(pop)                        

# create new df statePop
statePop <- data.frame(state, pop)            

# create new df by merging df's medianIncome, stateIncome
dfStates <- merge(medianIncome, statePop, by="state")  
str(dfStates)
head(dfStates)

# R data set - state.abb
str(state.abb)
head(state.abb)

# match our dataframe and the state abbreviations
match(dfStates$state,state.abb)
# check for NA's
any(is.na(dfStates))

# bring in full state name from abbreviated state name
dfStates$stateName <- state.name[match(dfStates$state,state.abb)]
str(dfStates)
head(dfStates)

# send to lowercase
dfStates$stateName <- tolower(dfStates$stateName)
head(dfStates)

# drop rows for AK, HI, and DC and store in a new df 
dfConus <- subset(dfStates, state != "AK" & state != "HI" & state != "DC")


## Step 2:  Show the income and population per state
# add lat/lon bounding box per state
us <- map_data("state")
# plot the income data on the US map
ggplot(dfConus, aes(map_id = stateName)) + 
  geom_map(map = us, aes(fill = income)) + 
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + 
  ggtitle("Average Median Income of the Contiguous U.S.")

# plot the population data on the us map
ggplot(dfConus, aes(map_id = stateName)) + 
  geom_map(map = us, aes(fill = pop)) + 
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + 
  ggtitle("Population of the Contiguous U.S.")


## Step 3:  Show the income per zip code
# match with state names
dfMedianZip$stateName <- state.name[match(dfMedianZip$state,state.abb)]
# send to lowercase
dfMedianZip$stateName <- tolower(dfMedianZip$stateName)
# take a look
head(dfMedianZip)

# drop rows for AK, HI, and DC and store in a new df 
dfMedianZipConus <- subset(dfMedianZip, state != "AK" & state != "HI" & state != "DC")

# plot income per zip code for the contiguous US
ggplot(dfMedianZipConus, 
       aes(map_id = stateName)) + 
  geom_map(map = us, fill="black", color="white") + 
  expand_limits(x = us$long, y = us$lat) + 
  geom_point(data = dfMedianZipConus, aes(x = longitude, y = latitude, color = Median)) + 
  coord_map() + 
  ggtitle("Income per zip code")


## Step 4:  Show zip code density
# plot income density by zip code for the contiguous US
ggplot(dfMedianZipConus, 
       aes(map_id = stateName)) + 
  geom_map(map = us, fill="black", color="white") + 
  expand_limits(x = us$long, y = us$lat) + 
  geom_point(data = dfMedianZipConus, aes(x = longitude, y = latitude, color = Median)) + 
  coord_map() + 
  ggtitle("Income density by zip code") +
  geom_density_2d(data = dfMedianZipConus, aes(x = longitude, y = latitude))


# subsetting map
# set where we want our map to be centered
latlon <- geocode_OSM("New York City, NY",
                      return.first.only = TRUE,
                      as.data.frame =  TRUE,
                      server = "http://nominatim.openstreetmap.org")



## Step 5:  Zoom into the region around NYC
# plot the income per zip code for the contiguous US centered around NYC
ggplot(dfMedianZipConus, aes(map_id = stateName)) + 
  geom_map(map=us, fill="black", color="white") + 
  expand_limits(x =us$long, y = us$lat) + 
  geom_point(data = dfMedianZipConus, aes(x = longitude, y = latitude, color = Median)) + 
  coord_map() + 
  ggtitle("Income per zip code") + 
  geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size = 3) + 
  xlim(latlon$lon-10, latlon$lon+10) + 
  ylim(latlon$lat-10,latlon$lat+10)

# plot the income density per zip code centered around NYC
ggplot(dfMedianZipConus, aes(map_id = stateName)) + 
  geom_map(map=us, fill="black", color="white") + 
  expand_limits(x =us$long, y = us$lat) + 
  geom_point(data = dfMedianZipConus, aes(x = longitude, y = latitude, color = Median)) + 
  coord_map() + 
  ggtitle("Income per zip code") +
  geom_density_2d(data = dfMedianZipConus, aes(x = longitude, y = latitude)) +
  geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size = 3) + 
  xlim(latlon$lon-10, latlon$lon+10) + 
  ylim(latlon$lat-10,latlon$lat+10)





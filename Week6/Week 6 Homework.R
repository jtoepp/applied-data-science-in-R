#      Course:          IST687
#      Name:            Jeremy Toepp
#      Homework:        Week 6
#      Due Date:        5/10/2020
#      Date Submitted:  5/10/2020
#

# ---------- HW6: Intro -----------
# set working directory for the current project
setwd(choose.dir())

# include dependencies
library(dplyr)
library(tidyverse)
library(ggplot2)
library(imputeTS)
library(tidyr)
library(reshape2)
library(grDevices)

# set save location for the graphs
pathGraphs = "./Plots"

## Step 1:  Load the data
myAirQuality <- airquality

# take a look at the structure
str(myAirQuality)


## Step 2:  Clean the data
# check for NA's
any(is.na(myAirQuality))

# check for column names where NA's exist
colnames(myAirQuality)[apply(myAirQuality, 2, anyNA)]

# replace NA's with the column mean
dfAQ <- data.frame(na_mean(myAirQuality))

## Step 3:  Understand the data distribution
# histograms
# tested the below code and got it working successfully, but changed to a function for reproducibility
ggplot(dfAQ,aes(x = Ozone, fill = cut_interval(Ozone, n = 50))) + 
  geom_histogram(show.legend = FALSE, bins = 25) + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ozone Histogram") 
  ggsave("Fig01.png",
         path = pathGraphs)

ggplot(dfAQ,aes(x=Solar.R, fill = cut_interval(Solar.R, n = 50))) + 
  geom_histogram(show.legend = FALSE, bins = 25) + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Solar Histogram")
  ggsave("Fig02.png",
         path = pathGraphs)

ggplot(dfAQ,aes(x=Wind, fill = cut_interval(Wind, n = 50))) + 
  geom_histogram(show.legend = FALSE, bins = 25) + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wind Histogram") +
  ggsave("Fig03.png",
         plot = last_plot(),
         path = pathGraphs)

ggplot(dfAQ,aes(x=Temp, fill = cut_interval(Temp, n = 50))) + 
  geom_histogram(show.legend = FALSE, bins = 25) + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Temperature Histogram") +
  ggsave("Fig04.png",
         plot = last_plot(),
         path = pathGraphs)

# function for histograms -- attempted but could not get titles to pass variables
# gg_hist <- function(df, val) {
#   label = colnames(val)
#   ggplot(df,aes(x = val, fill = cut_interval(val, n = 50))) + 
#     geom_histogram(show.legend = FALSE, bins = 25) + 
#     theme_dark() +
#     labs(title = "Air Quality Dataset (1973)",
#          subtitle = label)
# }

# use the function--must specify the column name with $
# gg_hist(dfAQ, dfAQ$Ozone)
# gg_hist(dfAQ, dfAQ$Solar.R)
# gg_hist(dfAQ, dfAQ$Wind)
# gg_hist(dfAQ, dfAQ$Temp)


# ozone boxplot
ggplot(dfAQ, aes(x = factor(0), y = Ozone)) + 
  geom_boxplot(fill = "lightgreen") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ozone Boxplot") +
  ggsave("Fig05.png",
         plot = last_plot(),
         path = pathGraphs)

# wind boxplot
ggplot(dfAQ, aes(x = factor(0), y = Wind)) + 
  geom_boxplot(fill = "lightblue") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wind Boxplot") +
  ggsave("Fig06.png",
         plot = last_plot(),
         path = pathGraphs)


## Step 4:  Explore how the data changes over time
# add the year of the dataset
dfAQ$Year <- 1973

# add a new date column combining Month, Day, Year columns and store in new dataframe
test_dfAQ <- unite(dfAQ, Date, c(Month, Day, Year), remove = FALSE)

# convert Date column to Date format
test_dfAQ$Date <- as.Date(test_dfAQ$Date, format="%m_%d_%Y")

# verify correct Date class for Date column
str(test_dfAQ$Date)

# now that it has been verified, store in original dataframe
dfAQ <- test_dfAQ

# individual line charts using ggplot with customized graphics
ggplot(dfAQ, aes(x = Date, y = Ozone)) + 
  geom_line(color = "lightgreen") + 
  geom_point(color = "green") + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ozone Line Graph/Scatter Plot") +
  ggsave("Fig07.png",
         plot = last_plot(),
         path = pathGraphs)

ggplot(dfAQ, aes(x = Date, y = Solar.R)) + 
  geom_line(color = "red3") + 
  geom_point(color = "yellow3") + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Solar Radiation Line Graph/Scatter Plot") +
  ggsave("Fig08.png",
         plot = last_plot(),
         path = pathGraphs)

ggplot(dfAQ, aes(x = Date, y = Wind)) + 
  geom_line(color = "purple") + 
  geom_point(color = "pink") + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wind Line Graph/Scatter Plot") +
  ggsave("Fig09.png",
         plot = last_plot(),
         path = pathGraphs)

ggplot(dfAQ, aes(x = Date, y = Temp)) + 
  geom_line(color = "lightblue") + 
  geom_point(color = "orange") + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Temperature Line Graph/Scatter Plot") +
  ggsave("Fig10.png",
         plot = last_plot(),
         path = pathGraphs)

# combined line charts -- could have also used melt() function
# remove extraneous columns and create a dataframe that is more conducive to plotting on one graph
dfAQ_long <- dfAQ %>%
  select(-Month, -Day, -Year) %>%
  tidyr::gather(response, value, -Date)

# check for "tidyness"--helps when working with multiple variables that share the same date but different values
# basically, verifying that each variable still have an equal number of observations
table(dfAQ_long$response)

# rename column names using tidyverse
dfAQ_long <- dfAQ_long %>%
  rename(
    Variable = response,
    Value = value
  )

# alter so that wind is scaled by 10 as requested
dfAQ_scaledWind <- dfAQ
dfAQ_scaledWind$Wind <- dfAQ_scaledWind$Wind * 10

# repeat above steps to plot with the scaled wind
# remove extraneous columns and create a dataframe that is more conducive to plotting on one graph
dfAQ_long_scaledWind <- dfAQ_scaledWind %>%
  select(-Month, -Day, -Year) %>%
  tidyr::gather(response, value, -Date)

# plot all on one graph
ggplot(dfAQ_long_scaledWind,
       aes(x = Date,
           y = value,
           linetype = response,
           color = response)) +
  geom_line(size = 1, linetype = 1) + 
  theme_dark() +
  labs(title = "Air Quality Dataset (1973)",
       subtitle = "Overlapping plots of each variable* in the dataset",
       caption = "*wind has been scaled by 10 to have a similar scale as the other variables") +
  ggsave("Fig11.png",
         plot = last_plot(),
         path = pathGraphs)

# playing around with facets
ggplot(dfAQ_long, 
       aes(x = Date,
           y = Value,
           linetype = Variable,
           color = Variable)) +
  geom_smooth(method = "loess", linetype = 1, color = "lightgray") +
  geom_line(linetype = 1) +
  facet_wrap(~Variable, scales="free_y", nrow=2, strip.position="top") +
  theme_dark() +
  labs(name = "Variable",
       title = "Air Quality Dataset (1973)",
       subtitle = "Individual plots of each variable utilizing 'loess'
  - t-based approximation for a smoothing curve and confidence interval",
       caption = "Units:
       Ozone (parts per billion)
       Solar (Watts per square meter)
       Wind (Miles per Hour)
       Temp (degrees Fahrenheit)",
       ylab = "Values*") +
  ggsave("Fig12.png",
         plot = last_plot(),
         path = pathGraphs)

## Step 4:  Look at all the data via a heatmap
ggplot(dfAQ_long,
       aes(x = Date,
           y = Variable)) +
  geom_tile(aes(fill = Value)) +
  scale_fill_gradient(low="white", high="blue") +
  ggsave("Fig13.png",
         plot = last_plot(),
         path = pathGraphs)

## Step 5:  Look at all the data via a scatter chart
ggplot(dfAQ, aes(x = Wind, y = Temp)) +
  geom_point(aes(size = Ozone, color = Solar.R)) +
  theme_dark() +
  ggsave("Fig14.png",
         plot = last_plot(),
         path = pathGraphs)

## Step 6:  Final Analysis
## Patterns
# All the variables had a seasonality to them, but it would have been much better to see
# the entire year or, much better, to see a running average of all years. This would greatly
# reduce the spikes in Ozone an Solar, and could see weekly averages rather than daily values.
# What I found most interesting was that Ozone peaked in mid-August while Solar Radiation 
# peaked in mid-June. That would make more sense, though, considering where the most Direct
# Normal Irradience (DNI) is highest during the Summer. This is, however, also without 
# considering any cloud cover that drastically affects how much irradiance reaches the
# surface. Temperature follows the natural seasonal curve, with a noticeable out-of-season 
# spike in temperature mid=September. Ozone follows an expected trend to match the
# temperature. Wind also follows expected patterns for each season, with frontal activity 
# the most likely impact to the general increases in the trendline, and the noted lower 
# activity during the summer aligning with the retreat of the maritime polar climate and
# an advancement of the maritime tropical climate. Additional analysis would be desired to 
# explore deeper and/or longer trends.

## Visualization
# The most useful visualization was not the final scatter plot, like I thought it would be, 
# but rather the individual scatter plots I quite liked the way that the heat map makes for
# easy visualization of the changes in variables. That is why I added the facet_wrap plot
# to visualize all of the data at once.  I especially liked being able to use the free_y
# to enable each variable to keep their own relativity, without any need for scaling. It 
# therefore preserves the data and allows it all to be viewed effectively. The smoothing
# curve also helps in better visualizing the data without all the many peaks and valleys.
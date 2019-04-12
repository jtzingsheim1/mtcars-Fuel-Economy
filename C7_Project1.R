# Coursera Data Science Specialization Course 7 Project 1 Script


# The purpose of this script is to complete the basic requirements behind the
# project 1 peer-graded assignment which is part of the Regression Models course
# from Johns Hopkins University within the Data Science Specialization on
# Coursera.
#
# The instructions say to perform exploratory data analysis and use regression
# models with the mtcars data set to answer these questions:
# 1. Is an automatic or manual transmission better for fuel economy
# 2. Quantify the impact of transmission type on fuel economy
#
# The input for this document is the mtcars data set which comes as part of the
# datasets package in base R. There is no specific output for this script, the
# script will just be used as a starting point for creating a markdown file
# which will later be used to create a final report.


library(datasets)
library(dplyr)


## Part 1) Loading, exploring, and preprocessing the data

data("mtcars")  # Load the mt cars data set into workspace as an object mtcars
mtcars <- as_tibble(mtcars)  # Convert from data frame to a tbl_df object
#print(str(mtcars))  # Check out the variables and preview some values
# The variables are mostly self-explanatory but checking the help file is useful
# mpg	= Miles/(US) gallon
# cyl	= Number of cylinders
# disp = Displacement (cu.in.)
# hp = Gross horsepower
# drat = Rear axle ratio
# wt = Weight (1000 lbs)
# qsec = 1/4 mile time
# vs = Engine (0 = V-shaped, 1 = straight)
# am = Transmission (0 = automatic, 1 = manual)
# gear = Number of forward gears
# carb = Number of carburetors

# All of the variables begin as the type numeric, but many are not continuous
# and for the purpose of this analysis it will be more useful to think of them
# as factors instead. These will be converted to factors below.
mtcars <- mtcars %>%
  mutate(cyl = as.factor(cyl)) %>%
  mutate(vs = as.factor(vs)) %>%
  mutate(am = as.factor(am)) %>%
  mutate(gear = as.factor(gear)) %>%
  mutate(carb = as.factor(carb))

# The vs and am variables are coded with ones and zeros, but it will be easier
# to work with them by the names those values represent:
levels(mtcars$vs) <- c("V-shaped", "Straight")
levels(mtcars$am) <- c("Automatic", "Manual")
    
# Make some plots to check out the data
par(mfrow = c(1, 2))  # Setup plot space
boxplot(mpg ~ am, data = mtcars, ylab = "mpg [mpg]")  # mpg vs transmission type
boxplot(wt ~ am, data = mtcars, ylab = "weight [klbs]")  # wt vs. trans type
# In the first plot above one could jump to the conclusion that mpg is clearly
# larger with manual transmission. However in the second plot one can see that
# manual transmission vehicles tend to be lighter in this dataset, and common
# sense suggests that mpg should be higher for lower weight vehicles. Many of
# these variables can be expected to correlate with mpg and/or with each other.
# The regression model must correct for these confounding effects and correctly
# isolate the effect of transmission on fuel economy.

# Exploratory plots
#boxplot(mtcars$mpg ~ mtcars$cyl)
#plot(mtcars$mpg ~ mtcars$disp)
#plot(mtcars$mpg ~ mtcars$hp)
#plot(mtcars$mpg ~ mtcars$drat)
#plot(mtcars$mpg ~ mtcars$wt)
#plot(mtcars$mpg ~ mtcars$qsec)
#boxplot(mtcars$mpg ~ mtcars$vs)
#boxplot(mtcars$mpg ~ mtcars$gear)
#boxplot(mtcars$mpg ~ mtcars$carb)
#plot(mtcars$hp ~ mtcars$disp)
#plot(hp ~ wt, data = mtcars)
#plot(hp ~ disp, data = mtcars)
#boxplot(mtcars$disp ~ mtcars$am, ylab = "Displacement [in^3]") 
#boxplot(mtcars$hp ~ mtcars$am, ylab = "Gross Horsepower [hp]")
#boxplot(mtcars$drat ~ mtcars$am, ylab = "Rear Axle Ratio") 
#boxplot(mtcars$qsec ~ mtcars$am, ylab = "Quarter Mile Time [s]")

# It is useful to pause here and think of the ways in which the variables might
# relate to each other and fuel economy:
# - By definition displacement = effective cylinder volume * number of cylinders
# so the cyl variable will have little use in this analysis since any
# information it contains will already be contained in displacement
# - While the relationship is not formulaic between displacement and horsepower,
# the two are expected to correlate since a higher displacement engine should
# produce more torque which should generally yield more horsepower. Checking
# this data set shows that hp is positively correlated with disp.
# - Horsepower can also be expected to correlate positively with vehicle weight
# since a higher weight vehicle will generally need more horsepower to function.
# Checking this data set shows that hp is positively correlated with wt.
# - Rear axle ratio can be expected to correlate with fuel economy
# - Weight can be expected to correlate with fuel economy
# - Quarter mile time is a result of many features of the car similar to mpg,
# while it can be expected to correlate with mpg, it may not be adding new
# information to the model
# Engine shape may have a small impact on fuel economy
# Number of gears and carburators may correlate with fuel economy

# Build a model using backward elimination method
fit_all <- lm(mpg ~ ., data = mtcars)
print(summary(fit_all))











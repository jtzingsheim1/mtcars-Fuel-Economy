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
# The variables are mostly self-explanatory, but checking the helpfile is useful
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
boxplot(mtcars$mpg ~ mtcars$am, ylab = "mpg [mpg]")  # mpg vs. transmission type
boxplot(mtcars$wt ~ mtcars$am, ylab = "weight [klbs]")  # wt vs. trans type
# In the first plot above one could jump to the conclusion that mpg is clearly
# higher with manual transmission. However in the second plot one can see that
# manual transmission vehicles tend to be lighter in this dataset, and common
# sense suggests that mpg should be higher for lower weight vehicles. Many of
# these variables can be expected to correlate with mpg and/or with each other.
# The regression model must correct for these confounding effects and correctly
# isolate the effect of transmission on fuel economy.







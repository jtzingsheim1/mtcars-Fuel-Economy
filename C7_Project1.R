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
library(car)


# Part 0) Function definitions

# Create a function to help with backward elimination modeling method
GetMaxPVariable <- function(model) {
  # Retrieves the variable with the largest p-value from a model
  #
  # Args:
  #   model: an object containing results returned by a model function (e.g. lm)
  #
  # Returns:
  #   The largest p-value from a model along with its variable name
  model.cf <- summary(model)$coefficients  # Get coefficients section
  cf.names <- attr(model.cf, "dimnames")[[1]]  # Get variable names
  model.cf <- as_tibble(model.cf)  # Convert to tibble object
  max.p.var <- model.cf %>%
    mutate(Variable = cf.names) %>%  # Add column for variable names
    rename(pvalue = "Pr(>|t|)") %>%
    select(Variable, pvalue) %>%  # Subset columns
    filter(pvalue == max(pvalue))  # Subset rows
  return(as.data.frame(max.p.var))
}


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

# All of the variables are originally loaded as numeric, but some are
# categorical and others are discrete. The categorical variables should be
# converted to 
mtcars <- mtcars %>%
  #mutate(cyl = as.factor(cyl)) %>%
  mutate(vs = as.factor(vs)) %>%
  mutate(am = as.factor(am)) #%>%
  #mutate(gear = as.factor(gear)) %>%
  #mutate(carb = as.factor(carb))

# The vs and am variables are coded with ones and zeros, but it will be easier
# to work with them by the names those values represent:
levels(mtcars$vs) <- c("V-shaped", "Straight")
levels(mtcars$am) <- c("Automatic", "Manual")
    
# Make some plots to check out the data
par(mfrow = c(1, 2))  # Setup plot space
#boxplot(mpg ~ am, data = mtcars, ylab = "mpg [mpg]")  # mpg vs transmission type
#boxplot(wt ~ am, data = mtcars, ylab = "weight [klbs]")  # wt vs. trans type
# In the first plot above one could jump to the conclusion that mpg is clearly
# larger with manual transmission. However in the second plot one can see that
# manual transmission vehicles tend to be lighter in this dataset, and common
# sense suggests that mpg should be higher for lower weight vehicles. Many of
# these variables can be expected to correlate with mpg and/or with each other.
# The regression model must correct for these confounding effects and correctly
# isolate the effect of transmission on fuel economy.

# Exploratory plots
boxplot(mtcars$mpg ~ mtcars$cyl)
plot(mtcars$mpg ~ mtcars$cyl)
#plot(mtcars$mpg ~ mtcars$disp)
#plot(mtcars$mpg ~ mtcars$hp)
#plot(mtcars$mpg ~ mtcars$drat)
#plot(mtcars$mpg ~ mtcars$wt)
#plot(mtcars$mpg ~ mtcars$qsec)
#boxplot(mtcars$mpg ~ mtcars$vs)
boxplot(mtcars$mpg ~ mtcars$gear)
plot(mtcars$mpg ~ mtcars$gear)
boxplot(mtcars$mpg ~ mtcars$carb)
plot(mtcars$mpg ~ mtcars$carb)
test1 <- lm(mpg ~ carb, data = mtcars)
test2 <- lm(mpg ~ as.factor(carb), data = mtcars)


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
# https://en.wikipedia.org/wiki/Engine_displacement
# - While the relationship is not formulaic between displacement and horsepower,
# the two are expected to correlate since a higher displacement engine should
# produce more torque which should generally yield more horsepower. Checking
# this data set shows that hp is positively correlated with disp.
# https://www.reddit.com/r/AskEngineers/comments/33h2em/how_are_engine_displacement_and_powertorque/
# - Horsepower can also be expected to correlate positively with vehicle weight
# since a higher weight vehicle will generally need more horsepower to function.
# Checking this data set shows that hp is positively correlated with wt.
# - Rear axle ratio can be expected to correlate with fuel economy
# https://shop.advanceautoparts.com/r/advice/car-technology/get-to-know-gear-ratios-and-how-they-affect-acceleration-and-mileage
# - Weight can be expected to correlate with fuel economy
# - Quarter mile time is a result of many features of the car similar to mpg,
# while it can be expected to correlate with mpg, it may not be adding new
# information to the model.
# - Engine shape may have a small impact on fuel economy
# - Number of gears and carburators may correlate with fuel economy

# Check VIFs
fit_all <- lm(mpg ~ ., data = mtcars)

# Build a model using backward elimination method
#print(GetMaxPVariable(fit_all))  # cyl has the highest p-value, eliminate it
#fit_b1 <- lm(mpg ~ disp + hp + drat + wt + qsec + vs + am + gear + carb,
#             data = mtcars)
#print(GetMaxPVariable(fit_b1))  # carb has the highest p-value, eliminate it
#fit_b2 <- lm(mpg ~ disp + hp + drat + wt + qsec + vs + am + gear, data = mtcars)
#print(GetMaxPVariable(fit_b2))  # gear has the highest p-value, eliminate it
#fit_b3 <- lm(mpg ~ disp + hp + drat + wt + qsec + vs + am, data = mtcars)
#print(GetMaxPVariable(fit_b3))  # vs has the highest p-value, eliminate it
#fit_b4 <- lm(mpg ~ disp + hp + drat + wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit_b4))  # drat has the highest p-value, eliminate it
#fit_b5 <- lm(mpg ~ disp + hp + wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit_b5))  # disp has the highest p-value, eliminate it
#fit_b6 <- lm(mpg ~ hp + wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit_b6))  # hp has the highest p-value, eliminate it
#fit_b7 <- lm(mpg ~ wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit_b7))  # Intercept has the highest p-value remaining

# Try the exercise again without including qsec
#fit_b8 <- lm(mpg ~ cyl + disp + hp + drat + wt + vs + am + gear + carb,
#             data = mtcars)
#print(GetMaxPVariable(fit_b8))  # carb has the highest p-value, eliminate it
#fit_b9 <- lm(mpg ~ cyl + disp + hp + drat + wt + vs + am + gear, data = mtcars)
#print(GetMaxPVariable(fit_b9))  # gear has the highest p-value, eliminate it
#fit_b10 <- lm(mpg ~ cyl + disp + hp + drat + wt + vs + am, data = mtcars)
#print(GetMaxPVariable(fit_b10))  # cyl has the highest p-value, eliminate it
#fit_b11 <- lm(mpg ~ disp + hp + drat + wt + vs + am, data = mtcars)
#print(GetMaxPVariable(fit_b11))  # drat has the highest p-value, eliminate it
#fit_b12 <- lm(mpg ~ disp + hp + wt + vs + am, data = mtcars)
#print(GetMaxPVariable(fit_b12))  # disp has the highest p-value, eliminate it
#fit_b13 <- lm(mpg ~ hp + wt + vs + am, data = mtcars)
#print(GetMaxPVariable(fit_b13))  # vs has the highest p-value, eliminate it
#fit_b14 <- lm(mpg ~ hp + wt + am, data = mtcars)
#print(GetMaxPVariable(fit_b14))  # am has the highest p-value remaining

# Try a more intelligent starting model
#fit_b15 <- lm(mpg ~ disp + disp * hp + hp + hp * wt + drat + wt + vs + am + gear + carb,
#              data = mtcars)
#print(GetMaxPVariable(fit_b15))  # vs has the highest p-value, eliminate it
#fit_b16 <- lm(mpg ~ disp + disp * hp + hp + hp * wt + drat + wt + am + gear + carb,
#              data = mtcars)
#print(GetMaxPVariable(fit_b16))  # Eliminate disp and the disp * hp int. factor
#fit_b17 <- lm(mpg ~ hp + hp * wt + drat + wt + am + gear + carb, data = mtcars)
#print(GetMaxPVariable(fit_b17))  # carb has the highest p-value, eliminate it
#fit_b18 <- lm(mpg ~ hp + hp * wt + drat + wt + am + gear, data = mtcars)
#print(GetMaxPVariable(fit_b18))  # am has the highest p-value, eliminate it
#fit_b19 <- lm(mpg ~ hp + hp * wt + drat + wt + gear, data = mtcars)
#print(GetMaxPVariable(fit_b19))  # drat has the highest p-value, eliminate it
#fit_b20 <- lm(mpg ~ hp + hp * wt + wt + gear, data = mtcars)
#print(GetMaxPVariable(fit_b20))  # gear has the highest p-value, eliminate it
#fit_b21 <- lm(mpg ~ hp + hp * wt + wt, data = mtcars)
#print(summary(fit_b21))  # All variables have significant p-values

# Check if adding any one variable can achieve a significant p-value

# Try building a model using forward selection without interaction variables
fit_f1 <- lm(mpg ~ cyl, data = mtcars)
fit_f2 <- lm(mpg ~ disp, data = mtcars)
fit_f3 <- lm(mpg ~ hp, data = mtcars)
fit_f4 <- lm(mpg ~ drat, data = mtcars)
fit_f5 <- lm(mpg ~ wt, data = mtcars)
fit_f6 <- lm(mpg ~ qsec, data = mtcars)
fit_f7 <- lm(mpg ~ vs, data = mtcars)
fit_f8 <- lm(mpg ~ am, data = mtcars)
fit_f9 <- lm(mpg ~ gear, data = mtcars)
fit_f10 <- lm(mpg ~ carb, data = mtcars)









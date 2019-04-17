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

# Create a function to help get p-values from models
SubsetModelSumm <- function(model) {
  # Subsets a model summary down to its variables and p-values
  #
  # Args:
  #   model: an object containing results returned by a model function (e.g. lm)
  #
  # Returns:
  #   A tibble object of the variables and p-values from summary(lm)
  model.cf <- summary(model)$coefficients  # Get coefficients section
  cf.names <- attr(model.cf, "dimnames")[[1]]  # Get variable names
  model.cf <- as_tibble(model.cf)  # Convert to tibble object
  model.summ <- model.cf %>%
    mutate(Variable = cf.names) %>%  # Add column for variable names
    rename(pvalue = "Pr(>|t|)") %>%
    select(Variable, pvalue)  # Subset columns
  return(model.summ)
}

GetMaxPVariable <- function(model) {
  # Retrieves the variable with the largest p-value from a model
  #
  # Args:
  #   model: an object containing results returned by a model function (e.g. lm)
  #
  # Returns:
  #   The largest p-value from a model along with its variable name
  model.summ <- SubsetModelSumm(model)
  max.p.var <- model.summ %>%
    filter(pvalue == max(pvalue))  # Subset rows
  return(as.data.frame(max.p.var))
}

# Create a function to help with forward selection modeling method
ShowPValues <- function(model.list) {
  # Show all variables and p-values from a list of models
  #
  # Args:
  #   model_list: an list containing model objects (e.g. output from lm)
  #
  # Returns:
  #   The variable names and p-values from all the models
  vars.table <- as_tibble()  # Need to size this or accomplish without loop
  for (i in seq_along(model.list)) {
    model.i <- model.list[[i]]  # Get a model from the list
    model.summ <- SubsetModelSumm(model.i)  # Subset down to vars and pvals
    model.summ <- model.summ %>%
      mutate(Model.Num = i) %>%  # Add col to ID the model
      select(Model.Num, Variable, pvalue)  # Reorder
    vars.table <- rbind(vars.table, model.summ)  # Trash
  }
  return(arrange(vars.table, pvalue))
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
# converted to factors, and the discrete variables should be assessed.
#par(mfrow = c(3, 2))  # Setup plot space
#boxplot(mtcars$mpg ~ mtcars$cyl)
#plot(mtcars$mpg ~ mtcars$cyl)  # Only three levels, but seems linear
#boxplot(mtcars$mpg ~ mtcars$gear)
#plot(mtcars$mpg ~ mtcars$gear)  # Only three levels, and doesn't seem linear
#boxplot(mtcars$mpg ~ mtcars$carb)
#plot(mtcars$mpg ~ mtcars$carb)  # Six levels, and somewhat linear, check models
#test.1 <- lm(mpg ~ carb, data = mtcars)  # Fit a model on carb as numeric
#test.2 <- lm(mpg ~ as.factor(carb), data = mtcars)  # Fit model on carb as cat.
#print(summary(test.1))  # R2 = 0.2803 and p-value = 0.0011
#print(summary(test.2))  # R2 = 0.3377 and p-value = 0.0065
#rm(test.1, test.2)
# Based on the checks above, cyl and carb will be treated as numeric and gear
# will be treated as a factor

# Convert data types
mtcars <- mtcars %>%
  mutate(vs = as.factor(vs)) %>%
  mutate(am = as.factor(am)) %>%
  mutate(gear = as.factor(gear))

# The vs and am variables are coded with ones and zeros, but it will be easier
# to work with them by the names those values represent:
levels(mtcars$vs) <- c("V-shaped", "Straight")
levels(mtcars$am) <- c("Automatic", "Manual")
    
# Make some plots to check out the data
par(mfrow = c(1, 2))  # Setup plot space
#boxplot(mtcars$mpg ~ mtcars$am, ylab = "mpg [mpg]")  # mpg vs trans type
#boxplot(mtcars$wt ~ mtcars$am, ylab = "weight [klbs]")  # wt vs. trans type
# In the first plot above one could jump to the conclusion that mpg is clearly
# higher with manual transmission. However in the second plot one can see that
# manual transmission vehicles tend to be lighter in this dataset, and common
# sense suggests that mpg should be higher for lower weight vehicles. Many of
# these variables can be expected to correlate with mpg and/or with each other.
# The regression model must correct for these confounding effects and correctly
# isolate the effect of the transmission type on fuel economy.

# Exploratory plots of mpg
#plot(mtcars$mpg ~ mtcars$disp)
#plot(mtcars$mpg ~ mtcars$hp)
#plot(mtcars$mpg ~ mtcars$drat)
#plot(mtcars$mpg ~ mtcars$wt)
#plot(mtcars$mpg ~ mtcars$qsec)
#boxplot(mtcars$mpg ~ mtcars$vs, ylab = "mpg [mpg]")

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
# while it can be expected to correlate with mpg, it is likely to be very
# correlated with all the other variables.
# - Engine shape, number of gears, and number of carburators may all have small
# correlations with fuel economy

# Exploratory plots of interactions
#boxplot(mtcars$disp ~ mtcars$am, ylab = "Displacement [in^3]") 
#plot(mtcars$hp ~ mtcars$disp)
#plot(mtcars$hp ~ mtcars$wt)
#boxplot(mtcars$hp ~ mtcars$am, ylab = "Gross Horsepower [hp]")
#boxplot(mtcars$drat ~ mtcars$am, ylab = "Rear Axle Ratio") 
#boxplot(mtcars$qsec ~ mtcars$am, ylab = "Quarter Mile Time [s]")

# Build a model using the backward elimination method
#fit.all <- lm(mpg ~ ., data = mtcars)  # Fit a model of all variables, no inter.
#fit.all <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb,
#              data = mtcars)
#print(GetMaxPVariable(fit.all))  # cyl
#fit.b0 <- lm(mpg ~ disp + hp + drat + wt + qsec + vs + am + gear + carb,
#              data = mtcars)
#print(GetMaxPVariable(fit.b0))  # carb
#fit.b1 <- lm(mpg ~ disp + hp + drat + wt + qsec + vs + am + gear, data = mtcars)
#print(GetMaxPVariable(fit.b1))  # gear
#fit.b2 <- lm(mpg ~ disp + hp + drat + wt + qsec + vs + am, data = mtcars)
#print(GetMaxPVariable(fit.b2))  # vs
#fit.b3 <- lm(mpg ~ disp + hp + drat + wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit.b3))  # drat
#fit.b4 <- lm(mpg ~ disp + hp + wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit.b4))  # disp
#fit.b5 <- lm(mpg ~ hp + wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit.b5))  # hp
#fit.b6 <- lm(mpg ~ wt + qsec + am, data = mtcars)
#print(GetMaxPVariable(fit.b6))  # Intercept
#fit.b7 <- lm(mpg ~ wt + qsec + am - 1, data = mtcars)
#print(GetMaxPVariable(fit.b7))  # am
#fit.b8 <- lm(mpg ~ wt + qsec - 1, data = mtcars)
#print(GetMaxPVariable(fit.b8))  # wt, but its significant
#print(summary(fit.b8))
#anova.b0 <- anova(fit.b8, fit.b7, fit.b6)  # Suggests to keep am and intercept
#anova.b1 <- anova(fit.b6, fit.b5, fit.b4, fit.b3, fit.b2, fit.b1, fit.b0,
#                  fit.all)  # Do not keep anything beyond the intercept
# Based on the sequence above model fit.b6 would be the best to use

# Original backward method on fit.all:
# cyl, carb, gear, vs, drat, disp, hp, Intercept

# Repeated, but starting model excluded qsec:
#fit.old1 <- lm(mpg ~ cyl + disp + hp + drat + wt + vs + am + gear + carb,
#              data = mtcars)
# carb, gear, cyl, drat, disp, vs, am

# Repeated again, but starting model excluded qsec, cyl, and included interact.
#fit.old2 <- lm(mpg ~ disp + disp * hp + hp + hp * wt + drat + wt + vs + am + gear + carb,
#              data = mtcars)
# vs, disp and disp * hp, carb, am, drat, gear
#fit.old2a <- lm(mpg ~ hp + hp * wt + wt, data = mtcars)
#print(summary(fit.old2a))  # All variables have significant p-values

# Build a model using forward selection without interaction variables
fit.f0 <- lm(mpg ~ cyl, data = mtcars)
fit.f1 <- lm(mpg ~ disp, data = mtcars)
fit.f2 <- lm(mpg ~ hp, data = mtcars)
fit.f3 <- lm(mpg ~ drat, data = mtcars)
fit.f4 <- lm(mpg ~ wt, data = mtcars)
fit.f5 <- lm(mpg ~ qsec, data = mtcars)
fit.f6 <- lm(mpg ~ vs, data = mtcars)
fit.f7 <- lm(mpg ~ am, data = mtcars)
fit.f8 <- lm(mpg ~ gear, data = mtcars)
fit.f9 <- lm(mpg ~ carb, data = mtcars)
models.f0 <- list(fit.f0, fit.f1, fit.f2, fit.f3, fit.f4, fit.f5, fit.f6,
                  fit.f7, fit.f8, fit.f9)
#print(ShowPValues(models.f0))  # wt has the lowest p-value
fit.f10 <- lm(mpg ~ wt + cyl, data = mtcars)
fit.f11 <- lm(mpg ~ wt + disp, data = mtcars)
fit.f12 <- lm(mpg ~ wt + hp, data = mtcars)
fit.f13 <- lm(mpg ~ wt + drat, data = mtcars)
fit.f14 <- lm(mpg ~ wt + qsec, data = mtcars)
fit.f15 <- lm(mpg ~ wt + vs, data = mtcars)
fit.f16 <- lm(mpg ~ wt + am, data = mtcars)
fit.f17 <- lm(mpg ~ wt + gear, data = mtcars)
fit.f18 <- lm(mpg ~ wt + carb, data = mtcars)
models.f1 <- list(fit.f10, fit.f11, fit.f12, fit.f13, fit.f14, fit.f15, fit.f16,
                  fit.f17, fit.f18)
print(ShowPValues(models.f1), n = 20)  # cyl has the lowest p-value
















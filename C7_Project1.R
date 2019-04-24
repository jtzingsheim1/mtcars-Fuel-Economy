# Coursera Data Science Specialization Course 7 Project 1 Script----------------
# Analysis of tranmission type's impact on fuel economy in the mtcars dataset


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
library(tidyverse)
#library(car)


# Part 0) Function definitions--------------------------------------------------

# Create a function to help get p-values from models
PValsOfModel <- function(model) {
  # Returns the variables and p-values from a model
  #
  # Args:
  #   model: an object containing results returned by a model function (e.g. lm)
  #
  # Returns:
  #   A tibble object of the variables and p-values from summary(lm)
  model %>%
    summary() %>%
    pluck("coefficients") %>%  # Get the coefficients table from a model summary
    as_tibble(rownames = "Variable") %>%
    rename(pvalue = "Pr(>|t|)") %>%
    select(Variable, pvalue)  # Subset to just the variable and pvalue columns
}

# Create a function to help with backward elimination modeling method
LargestPVar <- function(model) {
  # Retrieves the variable with the largest p-value from a model
  #
  # Args:
  #   model: an object containing results returned by a model function (e.g. lm)
  #
  # Returns:
  #   The largest p-value from a model along with its variable name
  model %>%
    PValsOfModel %>%
    filter(pvalue == max(pvalue)) %>%
    as.data.frame()
}

# Create a function to help with forward selection modeling method
SortVariables <- function(model.list) {
  # Finds which model has the lowest pvalue for each variable and sorts the vars
  #
  # Args:
  #   model_list: an list containing model objects (e.g. output from lm)
  #
  # Returns:
  #   A tibble showing which model had the lowest pvalue for each variable
  vars.list <- vector("list", length(model.list))  # Prepare a list to hold tbls
  for (i in seq_along(model.list)) {
    vars.list[[i]] <- model.list[[i]] %>%
      PValsOfModel() %>%
      mutate(Model.Num = i) %>%  # Add column to ID the model
      select(Model.Num, everything())
  }
  vars.list %>%
    bind_rows() %>%  # Assemble tibble from the list of tibbles
    group_by(Variable) %>%
    filter(pvalue == min(pvalue)) %>%  # Find model with best pvalue for ea var
    arrange(pvalue)  # Sort by increasing pvalues
}

# Create a function that builds formulas which can be used as input for lm
MakeLmInput <- function(response, predictors.unique, predictors.repeat = NULL) {
  # Builds formulas to be used as input for lm
  #
  # Args:
  #   response: A chr vector of the variable to be predicted by the others
  #   predictors.unique: A chr vector of variables to iterate (fit 1 model each)
  #   predictors.repeat: A chr vector of variables to appear in all models
  #
  # Returns:
  #
  if (is.null(predictors.repeat)) {
    repeated.portion <- paste(response, "~")
    separator <- " "
  } else {
    repeated.portion <- paste(predictors.repeat, collapse = " + ") %>%
      paste(response, ., sep = " ~ ")
    separator <- " + "
  }
  formula.list <- vector("list", length(predictors.unique))
  for (i in seq_along(predictors.unique)) {
    formula.list[[i]] <- paste(repeated.portion, predictors.unique[[i]],
                               sep = separator) %>%
      as.formula()
  }
  formula.list
}


# Part 1) Loading and preprocessing the data-----------------------------------

data("mtcars")
mtcars <- as_tibble(mtcars, rownames = "Vehicle")
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

# All of the original variables come through as numeric, but some are
# categorical and others are discrete. The categorical variables should be
# converted to factors, and the discrete variables should be assessed.
par(mfrow = c(3, 2))  # Setup plot space
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


# Part 2) Exploratory Data Analysis---------------------------------------------

# Things to check:
# 1. Check for missing values
# 2. Check variation of variables
# 3. Check covariation of variables

# 1. Check for missing values
#print(summary(mtcars))  # No NA values

# 2. Check variation of variables
# Things to consider:
# a. What values are common? Why?
# b. What values are rare? Why?
# c. Are there any unusual values? Why?
# d. Are there any patterns? Why.

# From the summary above, one can see disp, hp, and carb have means > median
par(mfrow = c(1, 2))  # Setup plot space
#hist(mtcars$mpg)  # Nothing unusual, fewer cars in 25 bin could be noise
#plot(as.factor(mtcars$cyl))  # Only three possible values: 4, 6, and 8
#hist(mtcars$disp)  # Right skew/flat with 4 obs per bin of 50 wide
#hist(mtcars$hp)  # Right skew, fewer observations of high hp
#hist(mtcars$drat)  # Looks normally distributed
#hist(mtcars$wt)  # Only one car with weight between 4k and 5k, and then 3 at 5k
#hist(mtcars$qsec)  # Looks normally distributed
# Engine shape is fairly evenly split: 18 & 14
# Transmission type is faily evenly split: 19 & 13
# Gear is mostly 3s and 4s, with only 5 observations in the 5 gear category
#plot(as.factor(mtcars$carb))  # Nearly all 1s, 2s, and 4s, then 1 each at 6 & 8

# 2. Check covariation of variables
# Things to consider:
# a. Could the pattern be due to chance?
# b. What relationship is implied by the pattern?
# c. How strong is the relationship?
# d. What other variables might be affecting the relationship?
# e. Does the relationship change for subgroups of the data?

# First consider the original question of how trans type relates to mpg:
#boxplot(mtcars$mpg ~ mtcars$am, ylab = "mpg [mpg]")  # mpg vs trans type
#boxplot(mtcars$wt ~ mtcars$am, ylab = "weight [klbs]")  # wt vs. trans type
# In the first plot above one could jump to the conclusion that mpg is clearly
# higher with manual transmission. However in the second plot one can see that
# manual transmission vehicles tend to be lighter in this dataset, and common
# sense suggests that mpg should be higher for lower weight vehicles. Many of
# these variables can be expected to correlate with mpg and/or with each other.
# The regression model must correct for these interactions and correctly
# isolate the effect of the transmission type on fuel economy.

# Check how mpg varies with the other variables
# It was shown above that mpg decreases with number of cylinders, as expected
#plot(mtcars$mpg ~ mtcars$disp)  # Negative slope, to be expected
#plot(mtcars$mpg ~ mtcars$hp)  # Negative slope, to be expected
#plot(mtcars$mpg ~ mtcars$drat)  # Positively sloped, opposite of expected
#plot(mtcars$mpg ~ mtcars$wt)  # Negative slope, very much expected
#plot(mtcars$mpg ~ mtcars$qsec)  # Generally positive, faster cars consume more
#plot(mtcars$mpg ~ mtcars$vs)  # Straight appears to have a significantly higher
# mpg, but such a strong trend is not expected
# It was shown above that mpg appears higher for manual cars, but this could be
# misleading based on the other factors
# It was shown above that there doesnt appear to be a strong trend with mpg vs
# number of gears, but one isn't really expected either
# It was shown above that there is a negative slope between mpg and carb, but
# such a strong relationship isn't really expected

# Follow up on a few interesting notes from above
# Why is mpg positively sloped with drat?
#plot(mtcars$wt ~ mtcars$drat)  # Negatively sloped, opposite of expected
#test <- lm(mpg ~ drat, data = mtcars)  # drat coefficient = 7.678
#test2 <- lm(mpg ~ drat + wt, data = mtcars)  # drat coefficient = 1.442
# Controlling for weight explains much of the effect of drat, but the slope is
# still positive, still unsure what explains this
#rm(test, test2)
# Now check why mpg shows such a strong response to engine shape
#plot(mtcars$wt ~ mtcars$vs)  # The v-shaped cars are noticebly heavier
#test <- lm(mpg ~ vs, data = mtcars)  # Straight coefficient is 7.94
#test2 <- lm(mpg ~ vs + wt, data = mtcars)  # Straight coefficient is 3.15
#rm(test, test2)
# Controlling for weight explains much of the effect of vs, but there is still a
# noticeable difference in mpg, perhaps further investigation is needed
# Now check why mpg appears to have a strong correlation with carb
#plot(mtcars$hp ~ mtcars$carb)  # Positively sloped
#plot(mtcars$wt ~ mtcars$carb)  # Positively sloped
#test <- lm(mpg ~ carb, data = mtcars)  # carb coefficient is -2.0557
#test2 <- lm(mpg ~ carb + hp, data = mtcars)  # carb coefficient is 0.26470
#test3 <- lm(mpg ~ carb + wt, data = mtcars)  # carb coefficient is -0.8215
#test4 <- lm(mpg ~ carb + hp + wt, data = mtcars)  # carb coefficient is -0.09288
#rm(test, test2, test3, test4)
# Controlling for hp and weight explains nearly all of the effect of carb

# Exploratory plots of interactions
#plot(mtcars$disp ~ mtcars$am)  # Manual have much lower disp, but the manual
# vehicles are lighter in this dataset, so it makes sense
#plot(mtcars$hp ~ mtcars$disp)  # Positive slope, to be expected
#plot(mtcars$hp ~ mtcars$wt)  # Positive slope, to be expected
#plot(mtcars$hp ~ mtcars$am)  # Lower for manual but expected if considering wt
#plot(mtcars$drat ~ mtcars$am)  # Higher for manual, but again the wt factor
#plot(mtcars$qsec ~ mtcars$am)  # Minimal difference, to be expected

# Also note:
# - By definition, displacement = effective cylinder volume * number of cyls
# - Quarter mile time is a result of many features of the car similar to mpg,
# while it can be expected to correlate with mpg, it is likely to be very
# correlated with all the other variables.

# Interaction effects to consider for analysis:
# - cyl and disp
# - disp and hp
# - disp and wt
# - disp and qsec
# - hp and wt
# - hp and carb
# - hp and qsec
# - drat and wt
# - drat and qsec
# - wt and qsec
# - wt and vs
# - wt and am
# - wt and gear
# - wt and carb
# - qsec and am
# - qsec and gear
# - qsec and carb


# Part 3) Model Selection-------------------------------------------------------

# Start building a model with the idea of forward selection in mind
# First check which variable has the strongest relationship with mpg by itself
all.variables <- names(mtcars)[-c(1:2)]  # Drop vehicle name and mpg variables
all.variables %>%
  MakeLmInput(response = "mpg") %>%  # Converts chr vector to lm input list
  map(lm, data = mtcars) %>%  # Fits a model to each variable
  SortVariables() %>%  # wt has the lowest p-value, start there
  print()
rm(all.variables)









# Old---------------------------------------------------------------------------

# Build a model using forward selection method
# Check individual models of all the main effects
#fit.f00 <- lm(mpg ~ cyl, data = mtcars)
#fit.f01 <- lm(mpg ~ disp, data = mtcars)
#fit.f02 <- lm(mpg ~ hp, data = mtcars)
#fit.f03 <- lm(mpg ~ drat, data = mtcars)
#fit.f04 <- lm(mpg ~ wt, data = mtcars)
#fit.f05 <- lm(mpg ~ qsec, data = mtcars)
#fit.f06 <- lm(mpg ~ vs, data = mtcars)
#fit.f07 <- lm(mpg ~ am, data = mtcars)
#fit.f08 <- lm(mpg ~ gear, data = mtcars)
#fit.f09 <- lm(mpg ~ carb, data = mtcars)
#models.f0 <- list(fit.f00, fit.f01, fit.f02, fit.f03, fit.f04, fit.f05, fit.f06,
#                  fit.f07, fit.f08, fit.f09)
#print(SortVariables(models.f0))  # wt from fit.f04 has the lowest p-value
#rm(fit.f00, fit.f01, fit.f02, fit.f03, fit.f05, fit.f06, fit.f07, fit.f08, 
#   fit.f09, models.f0)

# Check models of wt with all other main effects and associated interactions
#fit.f10 <- lm(mpg ~ wt + cyl, data = mtcars)
#fit.f11 <- lm(mpg ~ wt + disp, data = mtcars)
#fit.f12 <- lm(mpg ~ wt + disp + wt * disp, data = mtcars)
#fit.f13 <- lm(mpg ~ wt + hp, data = mtcars)
#fit.f14 <- lm(mpg ~ wt + hp + wt * hp, data = mtcars)
#fit.f15 <- lm(mpg ~ wt + drat, data = mtcars)
#fit.f16 <- lm(mpg ~ wt + drat + wt * drat, data = mtcars)
#fit.f17 <- lm(mpg ~ wt + qsec, data = mtcars)
#fit.f18 <- lm(mpg ~ wt + qsec + wt * qsec, data = mtcars)
#fit.f19 <- lm(mpg ~ wt + vs, data = mtcars)
#fit.f110 <- lm(mpg ~ wt + vs + wt * vs, data = mtcars)
#fit.f111 <- lm(mpg ~ wt + am, data = mtcars)
#fit.f112 <- lm(mpg ~ wt + am + wt * am, data = mtcars)
#fit.f113 <- lm(mpg ~ wt + gear, data = mtcars)
#fit.f114 <- lm(mpg ~ wt + gear + wt * gear, data = mtcars)
#fit.f115 <- lm(mpg ~ wt + carb, data = mtcars)
#fit.f116 <- lm(mpg ~ wt + carb + wt * carb, data = mtcars)
#models.f1 <- list(fit.f10, fit.f11, fit.f12, fit.f13, fit.f14, fit.f15, fit.f16,
#                  fit.f17, fit.f18, fit.f19, fit.f110, fit.f111, fit.f112,
#                  fit.f113, fit.f114, fit.f115, fit.f116)
#ShowPValues(models.f1) %>%
#  filter(Variable != "wt") %>%
#  print()  # hp from fit.f14 is next lowest
# Check the anova to verify significance
#print(anova(fit.f04, fit.f13, fit.f14))  # Keep hp and the interaction factor
#rm(fit.f10, fit.f11, fit.f12, fit.f13, fit.f15, fit.f16, fit.f17, fit.f18, fit.f19,
#   fit.f110, fit.f111, fit.f112, fit.f113, fit.f114, fit.f115, fit.f116,
#   models.f1)

# Third round of forward selection
#fit.f20 <- lm(mpg ~ wt + hp + wt * hp + cyl, data = mtcars)
#fit.f21 <- lm(mpg ~ wt + hp + wt * hp + disp, data = mtcars)
#fit.f22 <- lm(mpg ~ wt + hp + wt * hp + disp + disp * hp, data = mtcars)
#fit.f23 <- lm(mpg ~ wt + hp + wt * hp + disp + disp * wt, data = mtcars)
#fit.f24 <- lm(mpg ~ wt + hp + wt * hp + disp + disp * hp + disp * wt,
#              data = mtcars)
#fit.f25 <- lm(mpg ~ wt + hp + wt * hp + drat, data = mtcars)
#fit.f26 <- lm(mpg ~ wt + hp + wt * hp + drat + drat * wt, data = mtcars)
#fit.f27 <- lm(mpg ~ wt + hp + wt * hp + qsec, data = mtcars)
#fit.f28 <- lm(mpg ~ wt + hp + wt * hp + qsec + qsec * hp, data = mtcars)
#fit.f29 <- lm(mpg ~ wt + hp + wt * hp + qsec + qsec * wt, data = mtcars)
#fit.f210 <- lm(mpg ~ wt + hp + wt * hp + qsec + qsec * hp + qsec * wt,
#               data = mtcars)
#fit.f211 <- lm(mpg ~ wt + hp + wt * hp + vs, data = mtcars)
#fit.f212 <- lm(mpg ~ wt + hp + wt * hp + vs + wt * vs, data = mtcars)
#fit.f213 <- lm(mpg ~ wt + hp + wt * hp + am, data = mtcars)
#fit.f214 <- lm(mpg ~ wt + hp + wt * hp + am + wt * am, data = mtcars)
#fit.f215 <- lm(mpg ~ wt + hp + wt * hp + gear, data = mtcars)
#fit.f216 <- lm(mpg ~ wt + hp + wt * hp + gear + wt * gear, data = mtcars)
#fit.f217 <- lm(mpg ~ wt + hp + wt * hp + carb, data = mtcars)
#fit.f218 <- lm(mpg ~ wt + hp + wt * hp + carb + wt * carb, data = mtcars)
#models.f2 <- list(fit.f20, fit.f21, fit.f22, fit.f23, fit.f24, fit.f25, fit.f26,
#                  fit.f27, fit.f28, fit.f29, fit.f210, fit.f211, fit.f212,
#                  fit.f213, fit.f214, fit.f215, fit.f216, fit.f217, fit.f218)
#ShowPValues(models.f2) %>%
#  filter(Variable != "wt") %>%
#  filter(Variable != "hp") %>%
#  filter(Variable != "wt:hp") %>%
#  print()  # qsec from fit.f28 has the lowest p-value, but it is not signficant
# Check anova to verify significance
#print(anova(fit.f14, fit.f27, fit.f28))  # The p-values are not significant
#rm(fit.f04, fit.f20, fit.f21, fit.f22, fit.f23, fit.f24, fit.f25, fit.f26,
#   fit.f27, fit.f28, fit.f29, fit.f210, fit.f211, fit.f212, fit.f213, fit.f214,
#   fit.f215, fit.f216, fit.f217, fit.f218, models.f2)
# This round explored many additions to the model, but none made significant
# contributions to the model's performance
# The forward selection method converged on the same model as the backward
# elimination method.


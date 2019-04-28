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
library(car)  # For VIF function


# Part 0) Function definitions--------------------------------------------------

# Create a function to fit, summarize, and sort models from input variables
FitAndSortModels <- function(data, response, predictors.unique,
                             predictors.repeat = NULL) {
  # Assembles formulas to be used as input for lm, fits models, displays summary
  # statistics side-by-side, and sorts the result by decreasing adj. R-squared
  #
  # Args:
  #   data: the data set containing the variables, passed to lm
  #   response: A chr vector of the variable to be predicted by the others
  #   predictors.unique: A chr vector of variables to iterate (fit 1 model each)
  #   predictors.repeat: A chr vector of variables to repeat in all models
  #
  # Returns:
  #   A tibble containing the model formulas, model objects, and summary stats
  #
  # First build a vector of formulas from the character vectors of variables
  formula.vector <- MakeLmInput(response = response,
                                predictors.unique = predictors.unique,
                                predictors.repeat = predictors.repeat)
  # Next make a tibble which stores the model results alongside the formulas
  table.1 <- tibble(model.name = formula.vector) %>%
    mutate(model.object = map(model.name, lm, data = data))
  # Next make a tibble which collects summary statistics for each model
  table.2 <- table.1$model.object %>%  # List of the models
    map(GetModelStats) %>%  # Apply function to each element, output as list
    bind_rows()  # Condense the list down to a tibble
  # Lastly bind the two tibbles together and sort by decreasing adj. R-Squared
  result <- bind_cols(table.1, table.2) %>%
    arrange(desc(Ad.R.Squared))
  result  # Return the tibble
}

# Create a function to assemble lm formulas from input varibles
MakeLmInput <- function(response, predictors.unique, predictors.repeat = NULL) {
  # Assembles character vectors into formulas to be used as input for lm
  #
  # Args:
  #   response: A chr vector of the variable to be predicted by the others
  #   predictors.unique: A chr vector of variables to iterate (fit 1 model each)
  #   predictors.repeat: A chr vector of variables to appear in all models
  #
  # Returns:
  #   A character vector of "formulas" which can be coereced into actual
  #   formulas by lm.
  #
  # The pasting is slightly different depending if any variables will be
  # repeated in all the models, the if statement below handles this
  if (is.null(predictors.repeat)) {
    repeated.portion <- paste(response, "~")
    separator <- " "
  } else {
    repeated.portion <- paste(predictors.repeat, collapse = " + ") %>%
      paste(response, ., sep = " ~ ")
    separator <- " + "
  }
  # The variables are pasted below, the repeated portion is recycled for each of
  # the unique predictors. The output vector has length(predictors.unique)
  paste(repeated.portion, predictors.unique, sep = separator)
}

# Create a function to collect summary statistics for an individual model object
GetModelStats <- function(model) {
  # Returns a tibble of stats for given model object
  #
  # Args:
  #   model: an object containing results returned by a model function (e.g. lm)
  #
  # Returns:
  #   A tibble object of the R-Squared, Adj. R-Squared, and p-value of the model
  tibble(R.Squared = summary(model)$r.squared,
         Ad.R.Squared = summary(model)$adj.r.squared,
         model.pvalue = anova(model)$`Pr(>F)`[[1]])  # Easier to get from anova
}

# Part 1) Loading and preprocessing the data-----------------------------------

data("mtcars")
mtcars <- as_tibble(mtcars, rownames = "vehicle")
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
#par(mfrow = c(1, 2))  # Setup plot space
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

# Take a closer look at how mpg varies with wt when grouped by am
#ggplot(data = mtcars) +
#  geom_point(mapping = aes(x = wt, y = mpg, color = am))
# There is almost no overlap in the two groups. In fact, only two vehicles with
# automatic transmission have weights below 3.16, and only two vehicles with
# manual transmission have weights above 3.16.


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


# Part 3a) Model Selection Round 1----------------------------------------------

# Start building a model with the idea of forward selection in mind
# First check which variable has the strongest relationship with mpg by itself

# Collect all the predictor variables into a character vector
round1.vars <- mtcars %>%
  select(-vehicle, -mpg) %>%  # Drop the non-predictor variabels
  names()
# Fit, summarize, and sort a series of models
round1.table <- FitAndSortModels(data = mtcars, response = "mpg",
                                 predictors.unique = round1.vars)
#print(round1.table)  # mpg ~ wt is the top model
round1.model <- round1.table$model.object[[1]]  # Store the model object
rm(round1.vars, round1.table)

# Check out the residuals of the mpg ~ wt model
#hist(residuals(round1.model))  # Right skew, ranges from -4.54 to 6.87
#plot(residuals(round1.model) ~ mtcars$wt)  # V shape, high at ends low in center
#plot(mtcars$mpg ~ mtcars$wt)  # Original plot does show slight curvature

# Try some log transforms to see if it straightens out
#plot(log2(mtcars$mpg) ~ mtcars$wt)  # Improved compared to no transform
#plot(mtcars$mpg ~ log2(mtcars$wt))  # Improved compared to previous plot
#plot(log2(mtcars$mpg) ~ log2(mtcars$wt))  # Curved the opposite way now

# Based on the plots log2(wt) seems promising, but check the fits also
round1b.vars <- c("wt", "log2(wt)")
round1b.table1 <- FitAndSortModels(mtcars, "mpg", round1b.vars)
round1b.table2 <- FitAndSortModels(mtcars, "log2(mpg)", round1b.vars)
round1b.table <- bind_rows(round1b.table1, round1b.table2) %>%
  arrange(desc(Ad.R.Squared))
#print(round1b.table)  # mpg ~ log2(wt) outperforms original and other transforms
round1b.model <- round1b.table$model.object[[1]]  # Store the model object
rm(round1b.vars, round1b.table1, round1b.table2, round1b.table)

# Check the residuals of the transform model
#hist(residuals(round1b.model))  # Stronger right skew, ranges from -3.74 to 6.62
#plot(residuals(round1b.model) ~ mtcars$wt)  # V shape is lessened, wider

# Based on the checks above the log2(wt) is worth considering in future
# iterations of the model fitting, but the residual plots were not improved
# enough to jump to any conclusion about it. Add log2(wt) to the dataset below.
mtcars2 <- mutate(mtcars, "log2(wt)" = log2(wt))  # Add transform column
rm(round1.model, round1b.model)


# Part 3b) Model Selection Round 2----------------------------------------------

# The next round of models will consider interaction effects. It will also fit
# two series of models, one with wt and one with log2(wt)
round2a.interactions <- c("cyl * disp", "disp * hp", "disp * wt", "disp * qsec",
                          "hp * wt", "hp * carb", "hp * qsec", "drat * wt",
                          "drat * qsec", "wt * qsec", "wt * vs", "wt * am",
                          "wt * gear", "wt * carb", "qsec * am", "qsec * gear",
                          "qsec * carb")
round2a.vars <- mtcars2 %>%
  select(-vehicle, -mpg, -wt, -"log2(wt)") %>%  # Reduce to only repeated vars
  names() %>%  # Extract main variables
  c(round2a.interactions)  # Add in interaction effects
# Fit, summarize, and sort a series of models
round2a.table <- FitAndSortModels(data = mtcars2, response = "mpg",
                                 predictors.unique = round2a.vars,
                                 predictors.repeat = "wt")
#print(round2a.table)  # mpg ~ wt + hp + wt * hp is the top model
#round2a.model <- round2a.table$model.object[[1]]  # Store the model object
rm(round2a.interactions, round2a.vars)

# Now repeat but replacing wt with log2(wt)
round2b.interactions <- c("cyl * disp", "disp * hp", "disp * log2(wt)",
                          "disp * qsec", "hp * log2(wt)", "hp * carb",
                          "hp * qsec", "drat * log2(wt)", "drat * qsec",
                          "log2(wt) * qsec", "log2(wt) * vs", "log2(wt) * am",
                          "log2(wt) * gear", "log2(wt) * carb", "qsec * am",
                          "qsec * gear", "qsec * carb")
round2b.vars <- mtcars2 %>%
  select(-vehicle, -mpg, -wt, -"log2(wt)") %>%  # Reduce to only repeated vars
  names() %>%  # Extract main variables
  c(round2b.interactions)  # Add in interaction effects
# Fit, summarize, and sort a series of models
round2b.table <- FitAndSortModels(data = mtcars2, response = "mpg",
                                  predictors.unique = round2b.vars,
                                  predictors.repeat = "log2(wt)")
#print(round2b.table)  # mpg ~ log2(wt) + qsec * am is the top model
#round2b.model <- round2b.table$model.object[[1]]  # Store the model object
rm(round2b.interactions, round2b.vars)

round2.table <- bind_rows(round2a.table, round2b.table) %>%
  arrange(desc(Ad.R.Squared))
#print(round2.table)
rm(round2a.table, round2b.table)

# Im interested in the first 7 models
#print(map(round2.table$model.object[1:7], summary))
model.list <- round2.table$model.object[1:7]

# Model 1: mpg ~ log2(wt) + qsec + am + qsec * am
# This model has five terms, and the only three star coefficient is log2(wt).
# The intercept is a two star, and then qsec is a one star. The coefficients on
# amManual and qsec:amManual are above 0.1. It is worth checking Anova for this
# set of nested models to see which terms are justified
test1 <- lm(mpg ~ log2(wt), mtcars2)
test2 <- lm(mpg ~ log2(wt) + qsec, mtcars2)
test3 <- lm(mpg ~ log2(wt) + qsec + am, mtcars2)
test4 <- lm(mpg ~ log2(wt) + qsec + am + qsec * am, mtcars2)
#print(anova(test1, test2, test3, test4))  # test2 is the model to keep
rm(test1, test2, test3, test4)
# Anova suggests that the extra terms are not contributing enough to the model,
# so despite the R-Squared value of this model, it is no better than the simpler
# model of mpg ~ log2(wt) + qsec

# Model 2: mpg ~ disp + hp + disp * hp + log2(wt)
# This model has five terms, and the thre star coefficients are the intercept
# and log2(wt). hp is a two star, but disp and disp:hp are one star. Check Anova
# for this set of nested models
test1 <- lm(mpg ~ log2(wt), mtcars2)
test2 <- lm(mpg ~ log2(wt) + hp, mtcars2)
test3 <- lm(mpg ~ log2(wt) + hp + disp, mtcars2)
test4 <- lm(mpg ~ log2(wt) + hp + disp + disp * hp, mtcars2)
#print(anova(test1, test2, test3, test4))  # test2 is the model to keep
rm(test1, test2, test3, test4)
# Anova suggests that some of the extra terms are not justified, so this model
# should be reduced to the simpler mpg ~ log2(wt) + hp

# Model 3: mpg ~ hp + wt + hp * wt
# This model has four terms, and all of them are three stars, check Anova
test1 <- lm(mpg ~ wt, mtcars2)
test2 <- lm(mpg ~ wt + hp, mtcars2)
test3 <- lm(mpg ~ wt + hp + hp * wt, mtcars2)
#print(anova(test1, test2, test3))  # test3 is the model to keep
rm(test1, test2, test3)
# Anova confirms the inclusion of all these terms is justified

# Model 4: mpg ~ log2(wt) + qsec + log2(wt) * qsec
# This model has four terms, but none have any stars, check Anova
test1 <- lm(mpg ~ log2(wt), mtcars2)
test2 <- lm(mpg ~ log2(wt) + qsec, mtcars2)
test3 <- lm(mpg ~ log2(wt) + qsec + log2(wt) * qsec, mtcars2)
#print(anova(test1, test2, test3))  # test2 is the model to keep
rm(test1, test2, test3)
# Anova suggests that the interaction term is not justified, and this model
# could be reduced to mpg ~ log2(wt) + qsec

# Model 5: mpg ~ hp + log2(wt) + hp * log2(wt)
# This model has four terms - the intercept and log2(wt) have three stars, and
# hp has two stars. log2(wt):hp has no stars, check Anova.
test1 <- lm(mpg ~ log2(wt), mtcars2)
test2 <- lm(mpg ~ log2(wt) + hp, mtcars2)
test3 <- lm(mpg ~ log2(wt) + hp + log2(wt) * hp, mtcars2)
#print(anova(test1, test2, test3))  # test2 is the model to keep
rm(test1, test2, test3)
# Anova suggests that the interaction term is not justified, and this model
# could be reduced to mpg ~ log2(wt) + qsec

# Model 6: mpg ~ log2(wt) + qsec
# All three terms in this model have three stars. Previous Anova has shown this
# model doesnt need to be reduced.

# Model 7: mpg ~ hp + log2(wt) + qsec + hp * sec
# This model has five terms, and log2(wt) is the only three star term. None of
# the other terms have any stars. 
test1 <- lm(mpg ~ log2(wt), mtcars2)
test2 <- lm(mpg ~ log2(wt) + qsec, mtcars2)
test3 <- lm(mpg ~ log2(wt) + qsec + hp, mtcars2)
test4 <- lm(mpg ~ log2(wt) + qsec + hp + hp * qsec, mtcars2)
#print(anova(test1, test2, test3, test4))  # test2 is the model to keep
rm(test1, test2, test3, test4, model.list)
# Anova suggests that some of the extra terms are not justified, so this model
# should be reduced to the simpler mpg ~ log2(wt) + qsec

# The checks above show that the contenders for this round are:
# 6) mpg ~ log2(wt) + qsec
# 14) mpg ~ log2(wt) + hp
# 3) mpg ~ hp + wt + hp * wt
#print(round2.table[c(3, 6, 14), ])
# Of those three models, log2(wt) + hp can be eliminated

# Take a closer look at the mpg ~ hp * wt model
test1 <- lm(mpg ~ hp + wt + hp * wt, data = mtcars2)
# Check out the residuals
#hist(residuals(test1))  # Right skew, ranges from -3.06 to 4.55
#plot(residuals(test1) ~ mtcars$wt)  # Looks like random noise
#plot(residuals(test1) ~ mtcars$hp)  # Looks triangular
# See if there is a pattern to the observations with the largest residuals
#mtcars %>%
#  mutate(residuals = resid(test1) ^ 2) %>%
#  arrange(desc(residuals)) %>%
#  print()  # No pattern is evident

# Take a closer look at the residuals of the mpg ~ log2(wt) + qsec model
test2 <- lm(mpg ~ log2(wt) + qsec, data = mtcars2)
# Check the residuals
#hist(residuals(test2))  # Looks much more normal, ranges from -4.07 to 5.47
#plot(y = residuals(test2), x = log2(mtcars2$wt))  # Looks like random noise
#plot(residuals(test2) ~ mtcars2$qsec)  # Looks like random noise
# See if there is a pattern to the observations with the largest residuals
#mtcars2 %>%
#  mutate(residuals = resid(test2) ^ 2) %>%
#  arrange(desc(residuals)) %>%
#  print()  # No pattern is evident
rm(test1, test2, mtcars2, round2.table)

#print(round2.table[c(3, 6), ])
# At this point both models seem reasonable and have similar performance. All
# else being equal I favor the model without the qsec term since it is not a
# direct property of the car, it is a variable that is dependent on all the
# others, similar to mpg. Furthermore, all else being equal, I favor the model
# that does not transform wt ad the results are more easily interpreted. Lastly,
# the R.squared and adj. R.squared values are superior for the hp * wt model.
# Considering all these factors, I will select mpg ~ hp + wt + hp * wt as the
# winning model for this round. All that remains is to confirm if adding any
# additional variables can improve the model
round2.model <- lm(mpg ~ hp + wt + hp * wt, data = mtcars)


# Part 3c) Model Selection Round 3----------------------------------------------

# The base model from the previous round was: mpg ~ hp + wt + hp * wt
# The next round of models will consider several additions to the model,
# including interaction effects, to see if any improvements can be made
round3.interactions <- c("cyl * disp", "disp * hp", "disp * wt", "disp * qsec",
                         "hp * carb", "hp * qsec", "drat * wt", "drat * qsec",
                         "wt * qsec", "wt * vs", "wt * am", "wt * gear",
                         "wt * carb", "qsec * am", "qsec * gear", "qsec * carb")
round3.vars <- mtcars %>%
  select(-vehicle, -mpg, -wt, -hp) %>%  # Reduce to only repeated vars
  names() %>%  # Extract main variables
  c(round3.interactions)  # Add in interaction effects
# Fit, summarize, and sort a series of models
round3.table <- FitAndSortModels(data = mtcars, response = "mpg",
                                 predictors.unique = round3.vars,
                                 predictors.repeat = c("hp", "wt", "hp * wt"))
#print(round3.table)  # mpg ~ wt + hp + wt * hp is the top model
#round2a.model <- round2a.table$model.object[[1]]  # Store the model object
rm(round3.interactions, round3.vars)

# Check if any of the additions outperform the base model in Anova
model.list <- round3.table$model.object
result <- vector("numeric", length(model.list))
for(i in seq_along(model.list)) {
  result[[i]] <- anova(round2.model, model.list[[i]])$`Pr(>F)`[[2]]
}
round3.table %>%
  select(model.name) %>%
  mutate(anova.pvalue = result) %>%
  arrange(anova.pvalue) #%>%
#  print()
rm(model.list, i, result, round3.table)
# The check above shows that none of the models with additional factors can
# significantly improve the model, so it looks like the best model that can
# describe this data is mpg ~ hp + wt + hp * wt


# Part 4) Address the specific questions from the project instructions----------

# The first question is: "Is an automatic or manual transmission better for fuel
# economy." To address this the transmission type must be added back to the base
# model.
part4.model <- lm(mpg ~ hp + wt + hp * wt + am, data = mtcars)
#print(summary(part4.model))
# The coefficient on transmission type is not significant, and the magnitude of
# the coefficient is quite small. Check anova to confirm if inclusion of the
# variable is warranted.
#print(anova(round2.model, part4.model))  # p-value 0.93, am variable is insignificant
# The answer to this question is that when predicting fuel economy with this
# data set, once one controls for the impact of hp, wt, and the interaction of
# hp with wt, the effect of the transmission type is not statistically
# significant. If one includes the variable anyways there appears to be a 0.13
# mpg improvement when using a manual transimission instead of automatic, but
# this trend is likely due to chance.

# The second question was: "Quantify the impact of transmission type on fuel
# economy." As mentioned above the impact is not statistically significant when
# you control for the effects of the statistically significant predictors. If
# one fits a model anyways there is a 0.13 mpg improvement when using manual
# transmission instead of automatic, but this trend is likely due to chance.
# The expected values of mpg are shown below after accounting for the other vars
pred.mpg.auto <- c(int = 1, hp = mean(mtcars$hp), wt = mean(mtcars$wt), am = 0,
                   "hp:wt" = mean(mtcars$hp * mtcars$wt)) %>%
  "*" (coefficients(part4.model)) %>%
  sum()  # 20.04 mpg
pred.mpg.man <- pred.mpg.auto %>%
  "+" (coefficients(part4.model)[[4]])  # 20.16 mpg
rm(pred.mpg.auto, pred.mpg.man, part4.model)
# The difference can be attributed to random chance


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

#ybar <- coefficients(round2.model)[[1]] +
#  coefficients(round2.model)[[2]] * mean(mtcars$hp) +
#  coefficients(round2.model)[[3]] * mean(mtcars$wt) +
#  coefficients(round2.model)[[4]] * mean(mtcars$hp * mtcars$wt)


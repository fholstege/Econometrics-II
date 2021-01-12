

# Packages required for subsequent analysis. P_load ensures these will be installed and loaded. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

# load dataset
dfEarnings <- read.csv("Data/logEarnings.csv")

# set seed to reproduce analysis
set.seed(123)

###
# Exercise 1:
###

## Part A: Create a simple model with wages as dependent, schooling, age and age squared as independent
model1 = lm(logWage ~ schooling + age + age2, data = dfEarnings)
summary(model1)

## Comment: Only schooling is statistically significant, with a coefficient of 0.22
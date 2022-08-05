# Rahul Patel
# Date: 2022-02-08
# Project: CFA


# Packages ----------------------------------------------------------------
library(tidyverse)
library(lavaan)

# Load Data ---------------------------------------------------------------
assignment_data <- read_csv("cfaAssignmentData.csv")
glimpe(assignment_data)
str(assignment_data)


# Factor Models -----------------------------------------------------------

one_factor_model <-
'#Factors
f1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 
'

three_factor_model <-
'#Factors
f1 =~ Item1 + Item2 + Item3
f2 =~ Item4 + Item5 + Item6 
f3 =~ Item7 + Item8 + Item9
'

# Testing Fit -------------------------------------------------------------

## One Factor --------------------------------------------------------------
cfa_test_1 <- cfa(one_factor_model, data = assignment_data)
summary(cfa_test_1,
        standardized = T,
        fit.measures = T)
# item 1 is like that bc R is using the units of measurement in item 1 and it serves as a baseline
# always use model test user model, NOT baseline
# CFA =. 71
# RMSEA = .29 
# SRMR = .14
# Overall, our model is bad 

## Three Factor Factor --------------------------------------------------------------
cfa_test_3 <- cfa(three_factor_model, data = assignment_data)
summary(cfa_test_3,
        standardized = T,
        fit.measures = T)

# std.all are the factor loadings
# Model fit indices are looking okay...



# Comparing Factor Models -------------------------------------------------
anova(cfa_test_1, cfa_test_3)
# second line shows change in chi-square 




# Name: Rahul Patel
# Date: 2022-01-31
# Project: Assignment 2 


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(mediation)
library(apaTables)

# Loading Data File -------------------------------------------------------
assignment_data <- read_csv("multRegressionAssignmentData.csv")


# Inspect Data ------------------------------------------------------------
glimpse(assignment_data)
str(assignment_data)


# Part 1 ------------------------------------------------------------------
## Question 1 --------------------------------------------------------------

q1_regression <- lm(SaidYes ~ TrustProp + RiskProp, data = assignment_data)
summary(q1_regression)
apa.reg.table(q1_regression, filename = "q1_regression_table.doc")

## Question 2 --------------------------------------------------------------
# Answers for Q2 can be extracted from code in Q1

## Question 3 --------------------------------------------------------------
# First, remove NA values
assignment_data <- na.omit(assignment_data) # omit NA values

# Next, center the predictors (subtract each variable's mean from its values)

assignment_data$TrustProp_c <- scale(assignment_data$TrustProp, center = T, scale = F) # center

assignment_data$RiskProp_c <- scale(assignment_data$RiskProp, center = T, scale = F) # center

# Finally, run the regression, including the interaction term
q3_regression <- lm(SaidYes ~ TrustProp_c + RiskProp_c + I(TrustProp_c * RiskProp_c), data = assignment_data)

summary(q3_regression)
apa.reg.table(q3_regression, filename = "q3_regression_table.doc")


## Question 4 --------------------------------------------------------------
# Once you find an interaction, you look at simple main effects to test moderation
## where you examine the outcome at different levels of the moderator 
## accordingly, look +1SD and -1SD above and below the mean of the moderator

sd_RiskProp <- sd(assignment_data$RiskProp_c, na.rm = T)

# examine RiskProp_c at + 1SD (i.e. you took 1 SD away, which moves the intercept up)
assignment_data$RiskProp_plus_1 <- assignment_data$RiskProp_c - sd_RiskProp
assignment_data$RiskProp_minus_1 <- assignment_data$RiskProp_c + sd_RiskProp

# Regressions for high and low levels of RiskProp

high_RiskProp_regression <- lm(SaidYes ~ TrustProp_c + RiskProp_plus_1 + I(TrustProp_c*RiskProp_plus_1), data = assignment_data)

summary(high_RiskProp_regression)

apa.reg.table(high_RiskProp_regression, filename = "q3_moderation_high_RiskProp")


low_RiskProp_regression <- lm(SaidYes ~ TrustProp_c + RiskProp_minus_1 + I(TrustProp_c*RiskProp_minus_1), data = assignment_data)

summary(low_RiskProp_regression)

apa.reg.table(low_RiskProp_regression, filename = "q3_moderation_low_RiskProp")

## Question 5 --------------------------------------------------------------

# a path
pred_to_mediator <- lm(LikePeople ~ TrustProp, data = assignment_data)
summary(pred_to_mediator)
apa.reg.table(pred_to_mediator, filename = "med_reg_table_a.doc")

# b path
pred_to_outcome <- lm(Helpful ~ TrustProp + LikePeople, data = assignment_data)
summary(pred_to_outcome)
apa.reg.table(pred_to_outcome, filename = "med_reg_table_b.doc")

# c and c' path
set.seed(2022)
mediation_test <- mediate(pred_to_mediator,
                          pred_to_outcome,
                          boot=TRUE,treat="TrustProp",
                          mediator="LikePeople",
                          robustSE=TRUE,
                          sims=100)
summary(mediation_test)

# Part 2 -------------------------------------------------------------------
## Question 1 --------------------------------------------------------------

## Question 2 --------------------------------------------------------------

## Question 3 --------------------------------------------------------------


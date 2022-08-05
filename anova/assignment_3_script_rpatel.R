# Rahul Patel
# 2022-02-07
# ANOVA Assignment

# Packages ----------------------------------------------------------------
library(car)
library(MBESS)
library(tidyverse)
library(effectsize)
library(phia)

# Read Data ---------------------------------------------------------------
assignment_data <- read_csv("anovaAssignmentData.csv")

# Inspect Data ------------------------------------------------------------
glimpse(assignment_data)
str(assignment_data)


# Convert to Factors ------------------------------------------------------
assignment_data$WheelType <- as_factor(assignment_data$WheelType)
assignment_data$CoupleType <- as_factor(assignment_data$CoupleType)

levels(assignment_data$WheelType) <- list("Not Eccentric" = 1, "Eccentric" = 2)
levels(assignment_data$CoupleType) <- list("New Couple" = 1, "Established Couple" = 2)

# Set Options for ANOVA ---------------------------------------------------
options(contrasts = c("contr.helmert", "contr.poly")) # need to override default R options

# Assumption Testing ------------------------------------------------------
assignment_data$cells <- interaction(assignment_data$WheelType, assignment_data$CoupleType) # need this for Levene's test

leveneTest(assignment_data$Awkward, group = assignment_data$cells, center = "median") # tests homogeneity of variance assumption

# Descriptive Stats -------------------------------------------------------
psych::describeBy(assignment_data$Awkward, group = list(assignment_data$WheelType)) # describe awkwardness ratings by wheel type
psych::describeBy(assignment_data$Awkward, group = list(assignment_data$CoupleType)) # describe awkwardness ratings by couple type
psych::describeBy(assignment_data$Awkward, group = list(assignment_data$WheelType, assignment_data$CoupleType)) # describe awkwardness ratings by both couple and wheel types

# ANOVA -------------------------------------------------------------------

crf_lm <- lm(Awkward ~ WheelType*CoupleType, data = assignment_data)
Anova(crf_lm, type = 3) # put model object in this function to run ANOVA 

eta_squared(crf_lm, partial = TRUE, ci = .95, alternative = "two.sided") # get partial eta squared (effect size)


## Simple Effects ----------------------------------------------------------
testInteractions(crf_lm, fixed = "WheelType", across = "CoupleType", adjustment = "none") 
# to examine couple type at Eccentric wheel type
# wheel type is the moderator and couple type is the predictor

# a group where wheel type is eccentric and couple type is new couple
group_1 <- dplyr::filter(assignment_data,
                         WheelType == "Eccentric",
                         CoupleType == "New Couple")$Awkward

# a group where wheel type is eccentric and couple type is established couple
group_2 <- dplyr::filter(assignment_data,
                         WheelType == "Eccentric",
                         CoupleType == "Established Couple")$Awkward      


# Cohen's D ---------------------------------------------------------------

d_value <- smd(Group.1 = group_1, Group.2 = group_2, Unbiased = TRUE) # cohen's d
ci.smd(smd = d_value, n.1 = length(group_1), n.2 = length(group_2)) # 95% CIs for cohen's d



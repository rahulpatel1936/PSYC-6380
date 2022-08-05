# Rahul Patel
# Date: 2022-15-02
# Project: Chi Square and Logistic Regression Assignment


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(gmodels)
library(lsr)

# Load Data ---------------------------------------------------------------
lecture_data <- read_csv("catOutcomesAssignmentData.csv")
glimpse(lecture_data)

lecture_data$HadCoaching <- as.factor(lecture_data$HadCoaching)
levels(lecture_data$HadCoaching) <-list("Not Coached" = 1, "Coached" = 2)

lecture_data$Employed <- as.factor(lecture_data$Employed)
levels(lecture_data$Employed) <-list("No" = 0, "Yes" = 1)


# Chi Square --------------------------------------------------------------
CrossTable(lecture_data$HadCoaching,
           lecture_data$Employed,
           fisher = T,
           chisq = T,
           expected = T,
           sresid = T, # standardized residuals
           format = "SPSS")

# First row: Observed counts
# Second row: Expected counts
# Last row: standardized residuals
# For the chi square value, use the yates' correction version (the one at the bottom)

## Effect Size -------------------------------------------------------------

frequencies_not_coached <- c(73, 169)

frequencies_coached <- c(41, 217)

frequency_table <- cbind(frequencies_not_coached, frequencies_coached)

v_stat <- cramersV(frequency_table)
print(v_stat) # degree of association between the categorical variables
v_stat_squared <- v_stat^2 # proportion of variance accounted for 
print(v_stat_squared)


# Logistic Regression -----------------------------------------------------
log_regression_1 <- glm(Employed ~ PresentAbility, data = lecture_data, family = binomial())
summary(log_regression_1)
# coefficients is represented as logits
exp(log_regression_1$coefficients) # now we can interpret it as an odds ratio 
# for every one unit increase in PresentAbility, your likelihood of failing to make a Employed decreases by 6
exp(confint(log_regression_1))

# Proportion of Variance  -------------------------------------------------------------
#Calculate the pseudo-R2 index for the logistic regression above
chi_stat <- log_regression_1$null.deviance - log_regression_1$deviance  #calculate a chi-square value
print(chi_stat)

r_squared <-chi_stat/log_regression_1$null.deviance #calculate pseudo-R2 using the above chi-square value
print(r_squared) #calculate pseudo-R2 using the above chi-square value


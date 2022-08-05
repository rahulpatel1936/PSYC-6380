# Name: Rahul Patel
# Date: 2022/03/15
# Project: Assignment 8 

# Loading Packages --------------------------------------------------------
library(tidyverse)
library(lavaan)
library(magrittr)


# Read Data ---------------------------------------------------------------
assignment_data <- read_csv("semAssignmentData.csv")
glimpse(assignment_data)
str(assignment_data)
# can do SEM with factors; you just get a coeffient for every level of the factor
# you will just get dummy-coded results in the background


# SEM ---------------------------------------------------------------------


## Latent Path Analysis ----------------------------------------------------
path_analysis_1 <- '#Latent Factors
Negative Affect =~ NAffect1 + NAffect2 + NAffect3 + NAffect4 + NAffect5
Media Exposure =~ MediaExpose1 + MediaExpose2 + MediaExpose3
'
test_model_1 <- sem(path_analysis_1, data = assignment_data)
summary(test_model_1, 
        standardized = T,
        fit.measures = T,
        ci = T)
#std.all under Latent Variables are the factor loadings
# look at test user model 

standardizedsolution(test_model_1, ci = T)

path_analysis_2 <- '#Latent Factors
Negative Affect =~ NAffect1 + NAffect2 + NAffect4 
Media Exposure =~ MediaExpose1 + MediaExpose2 
#Regressions
FeltThreat ~ Negative Affect + Media Exposure
'
test_model_2 <- sem(path_analysis_2, data = assignment_data)
summary(test_model_2, 
        standardized = T,
        fit.measures = T,
        ci = T)
#std.all under Latent Variables are the factor loadings
# look at test user model 

standardizedsolution(test_model_2, ci = T)


## Mediation ------------------------------------------------
path_analysis_3 <- '#Latent Factors
Negative Affect =~ NAffect1 + NAffect2 + NAffect4 
Media Exposure =~ MediaExpose1 + MediaExpose2
#Regressions
Negative Affect~a*Media Exposure
FeltThreat~b*Negative Affect
FeltThreat~c*Media Exposure
#Custom
Indirect:=a*b
'
# first is predictor to mediator
# second is mediator to outcome
# third is predictor to outcome

test_model_3 <- sem(path_analysis_3, data = assignment_data)
summary(test_model_3,
        standardized = T,
        fit.measures = T,
        ci = T)

standardizedsolution(test_model_3, ci = T)
# indirect effect is a * b 

## Moderation ------------------------------------------------

cfa_1 <-'#Latent Factors
Negative Affect =~ NAffect1 + NAffect2 + NAffect4 
Media Exposure =~ MediaExpose1 + MediaExpose2
'
cfa_test_1 <-cfa(cfa_1, data = assignment_data)

assignment_data_merged <- data.frame(assignment_data, predict(cfa_test_1))

assignment_data_merged <- assignment_data_merged %>%
  mutate(Interaction = NegativeAffect*MediaExposure)

path_analysis_5 <- '
#Regressions
FeltThreat ~ Negative Affect + Media Exposure + Interaction
'
# don't need to specify latent threat and fear as it's in the data frame
test_model_5 <- sem(path_analysis_5, data = assignment_data_merged)
summary(test_model_5,
        standardized = T,
        fit.measures = T,
        ci = T)
standardizedsolution(test_model_5, ci = T)

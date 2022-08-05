# Name: Rahul 
# Date: 2022-03-22
# Project: Multilevel Modeling 


# Packages ----------------------------------------------------------------
library(lme4)
library(performance)
library(lmerTest)
library(tidyverse)
library(psych)

# Loading Data ------------------------------------------------------------
assignment_data <- read_csv("multilevelAssignmentData.csv")


# Specify null model -------------------------------------------------------
# 1 = intercept
# 1 | Location = allow intercept to vary across Locations

null_model <- lmer(Performance ~ 1 + (1 | Location),
                   data = assignment_data,
                   REML = F)

summary(null_model)
icc(null_model)



# Adding our Predictor ----------------------------------------------------
mlm_model_1 <- lmer(Performance ~ Humour +  (1 | Location),
                    data = assignment_data,
                    REML = F)
summary(mlm_model_1)
confint.merMod(mlm_model_1,
               oldNames = F)

# Adding Random effect of Humour by Location ------------------------------
mlm_model_2 <- lmer(Performance ~ Humour +  (Humour | Location),
                    data = assignment_data,
                    REML = F)
summary(mlm_model_2)


# Model Fit ---------------------------------------------------------------
summary(null_model) # pay note to AIC and BIC 
summary(mlm_model_1)
anova(mlm_model_2, mlm_model_1) # compare models with chi-square difference test
# chisq difference is 1640.90,  p < .001; the model fit is improved 


# Effect Size: Pseudo R Squared ------------------------------------------
r2(mlm_model_1) # conditional R2 includes fixed and random effects as predictors 
# the other one is just fixed effects as predictors


# Graphing  ---------------------------------------------------------------
assignment_data$predictedPerformance <- predict(mlm_model_1, assignment_data[1:nrow(assignment_data),], 
                                         allow.new.levels = T)
mlm_graph <- assignment_data %>%
  ggplot(aes(x = Humour, y = predictedPerformance, color = as.factor(Location))) + 
  geom_line(size=.3) +
  geom_point(aes(y=Performance))

ggsave(mlm_graph, filename = "mlm_graph.png")






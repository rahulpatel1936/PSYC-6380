# Name: Rahul Patel
# Date: April 10, 2022
# Project: Final Assignment

# Question 2  -------------------------------------------------------------

## Linear Regressions -------------------------------------------------------------
# Regression 1: Openness to New Experiences predicting Openness to Weird Experiences
glimpse(final_assignment_data)
glimpse(final_assignment_data_items)
glimpse(final_assignment_data_with_scores)

regression_1 <- lm(openness_weird_experiences ~ Openness, data = final_assignment_data_with_scores)
summary(regression_1)

apa.reg.table(regression_1,
              filename = "apa_reg_1_table.doc",
              table.number = 1)

# Regression 2: Openness to New Experiences predicting Number of Wacky Adventures

regression_2 <- lm(NumAdventures ~ Openness, data = final_assignment_data_with_scores)
summary(regression_2)

apa.reg.table(regression_2,
              filename = "apa_reg_2_table.doc",
              table.number = 2)

## Mediation ---------------------------------------------------------------

# a path
pred_to_mediator <- lm(openness_weird_experiences ~ Openness, data = final_assignment_data_with_scores)
summary(pred_to_mediator)
apa.reg.table(pred_to_mediator, filename = "med_reg_table_1.doc")

# b and c paths
pred_to_outcome <- lm(NumAdventures ~ Openness + openness_weird_experiences, data = final_assignment_data_with_scores)
summary(pred_to_outcome)
apa.reg.table(pred_to_outcome, filename = "med_reg_table_2.doc")

# c' path: indirect effect
set.seed(2022)
mediation_test <- mediate(pred_to_mediator,
                          pred_to_outcome,
                          boot=TRUE, treat="Openness",
                          mediator="openness_weird_experiences",
                          robustSE=TRUE,
                          sims=100)
summary(mediation_test)


?smd


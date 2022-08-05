# Name: Rahul Patel
# Date: April 10, 2022
# Project: Final Assignment

# Question 1  -------------------------------------------------------------

# PCA/EFA -----------------------------------------------------------------
final_assignment_data_items <- final_assignment_data %>% 
  dplyr::select(Adventure1:Adventure12)

efactor_analysis_1 <- psych::principal(final_assignment_data_items,
                                      nfactors = length(final_assignment_data_items),
                                      rotate = "none",
                                      scores = T)
efactor_analysis_1



efactor_analysis_2 <- psych::principal(final_assignment_data_items,
                                      nfactors = 2,
                                      rotate = "varimax",
                                      scores = T)
efactor_analysis_2

# SS loadings = Eigen values; 2 factors suggested
# When looking at loadings, look at largest correlations
# com is NOT communality. It's actually under the 'h2' column

## Scree Plot --------------------------------------------------------------
plot(efactor_analysis_2$values, type = "b") 

# Parallel Analysis
paran(final_assignment_data_items,
      cfa = F,
      graph = T,
      color = T)
# number of red dots above blue line or number of black dots above grey line

#Create a scree plot of the PCA results
plot(efactor_analysis_2$values,type="b")


# Scale Items -------------------------------------------------------------

risk_taking_propensity <- c("Adventure4", "Adventure5", "Adventure6", "Adventure8")

openness_weird_experiences <- c("-Adventure1", "Adventure2", "-Adventure3", "-Adventure7", "Adventure9", "Adventure10", "Adventure11", "Adventure12")

measure_list <- list(risk_taking_propensity = risk_taking_propensity, openness_weird_experiences = openness_weird_experiences)

scoring_key <- psych::make.keys(nvars = ncol(final_assignment_data_items),
                                keys.list = measure_list,
                                item.labels = names(final_assignment_data_items))
scoring_details <- psych::scoreItems(scoring_key,
                                     final_assignment_data_items,
                                     min = 1,
                                     max = 9,
                                     missing = T,
                                     impute = "none")
scoring_details$scores

final_assignment_data_with_scores <- cbind(final_assignment_data, scoring_details$scores)
final_assignment_data_with_scores


# Examine Distribution of Data --------------------------------------------
psych::describe(final_assignment_data_with_scores)
#Run a Shapiro-Wilk test to assess normality
shapiro.test(final_assignment_data_with_scores$Openness)
shapiro.test(final_assignment_data_with_scores$NumAdventures)
shapiro.test(final_assignment_data_with_scores$risk_taking_propensity)
shapiro.test(final_assignment_data_with_scores$openness_weird_experiences)



# Confirmatory Factor Analysis --------------------------------------------

# Factor Models -----------------------------------------------------------
## One Factor --------------------------------------------------------------
#Specify the proposed factor structure
one_factor_model <-
  '#Factors
f1 =~ Adventure4 + Adventure5 + Adventure6 + Adventure8 +
Adventure1 + Adventure2 + Adventure3 + Adventure7 + Adventure9 + Adventure10 + Adventure11 + Adventure12
'
#Run the factor analysis test
cfa_test_1<-cfa(one_factor_model, data = final_assignment_data_items) #run the test
summary(cfa_test_1,
        standardized=T,
        fit.measures=T) #review the results

## Two Factor --------------------------------------------------------------
two_factor_model <-
  '#Factors
f1 =~ Adventure4 + Adventure5 + Adventure6 + Adventure8
f2 =~ Adventure1 + Adventure2 + Adventure3 + Adventure7 + Adventure9 + Adventure10 + Adventure11 + Adventure12
'
#Run the factor analysis test
cfa_test_2 <- cfa(two_factor_model, data = final_assignment_data_items)
summary(cfa_test_2,
        standardized = T,
        fit.measures = T)

# item 1 is like that bc R is using the units of measurement in item 1 and it serves as a baseline
# always use model test user model, NOT baseline

#Test the change in model fit between the one-factor and two-factor models using a chi-square difference test
anova(cfa_test_1, cfa_test_2)





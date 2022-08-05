# Rahul Patel
# March 1, 2022
# Assignment 6
# Packages ----------------------------------------------------------------
library(tidyverse)
library(apaTables)
library(paran)

# Read Data ---------------------------------------------------------------
assignment_data <- read_csv("efaAssignmentData.csv")

glimpse(assignment_data)
str(assignment_data)

# Inter-item correlations -------------------------------------------------
observed_correlations <- cor(assignment_data)
print(observed_correlations)

# As an APA Table
apa.cor.table(data = assignment_data, filename = "inter_item_cor_table_assignment.doc")


# PCA/EFA -----------------------------------------------------------------
factor_analysis_1 <- psych::principal(assignment_data,
                                      nfactors = length(assignment_data),
                                      rotate = "none",
                                      scores = T)
factor_analysis_1
# SS loadings = Eigen values; 2 factors suggested
# When looking at loadings, look at largest correlations
# com is NOT communality. It's actually under the 'h2' column

## Scree Plot --------------------------------------------------------------
plot(factor_analysis_1$values, type = "b") 

# Parallel Analysis
paran(assignment_data,
      cfa = F,
      graph = T,
      color = T)
# number of red dots above blue line or number of black dots above grey line

# Re-do PCA  --------------------------------------------------------------
factor_analysis_2 <- psych::principal(assignment_data,
                                      nfactors = 2,
                                      rotate = "none",
                                      scores = T)
factor_analysis_2

# Re-do PCA  --------------------------------------------------------------
factor_analysis_3 <- psych::principal(assignment_data,
                                      nfactors = 2,
                                      rotate = "varimax",
                                      scores = T)
factor_analysis_3



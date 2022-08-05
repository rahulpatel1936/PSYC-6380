# Rahul Patel
# March 1, 2022
# Exploratory Factor Analysis


# Packages ----------------------------------------------------------------
library(tidyverse)
library(apaTables)
library(paran)

# Read Data ---------------------------------------------------------------
lecture_data <- read_csv("efaExampleData.csv")

glimpse(lecture_data)
str(lecture_data)

# Inter-item correlations -------------------------------------------------
observed_correlations <- cor(lecture_data)
print(observed_correlations)

# As an APA Table
apa.cor.table(data = lecture_data, filename = "inter_item_cor_table.doc")


# PCA/EFA -----------------------------------------------------------------
factor_analysis_1 <- psych::principal(lecture_data,
                                      nfactors = length(lecture_data),
                                      rotate = "none",
                                      scores = T)
factor_analysis_1
# SS loadings = Eigen values; 2 factors suggested
# When looking at loadings, look at largest correlations
# com is NOT communality. It's actually under the 'h2' column

## Scree Plot --------------------------------------------------------------
plot(factor_analysis_1$values, type = "b") 

# Parallel Analysis
paran(lecture_data,
      cfa = F,
      graph = T,
      color = T)
# number of red dots above blue line or number of black dots above grey line

# Re-do PCA  --------------------------------------------------------------
factor_analysis_2 <- psych::principal(lecture_data,
                                      nfactors = 2,
                                      rotate = "varimax",
                                      scores = T)
factor_analysis_2
# 1-6 on factor 1; 7-10 on factor 2



# Scale Items -------------------------------------------------------------

oddness <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")

danger <- c("Item7", "Item8", "Item9", "Item10")

measure_list <- list(odd_sit = oddness, dan_sit = danger)

scoring_key <- psych::make.keys(nvars = ncol(lecture_data),
                         keys.list = measure_list,
                         item.labels = names(lecture_data))
scoring_details <- psych::scoreItems(scoring_key,
                                     lecture_data,
                                     min = 1,
                                     max = 9,
                                     missing = T,
                                     impute = "none")
scoring_details$scores


new_data <- cbind(lecture_data, scoring_details$scores)
new_data

# Rahul Patel
# Date: 2022-02-08
# Project: Assumptions Testing Assignment 


# Loading Packages --------------------------------------------------------
library(tidyverse)


# Loading Data ------------------------------------------------------------
assignment_data <- read_csv("nonParametricAssignmentData.csv")


# Inspect Data ------------------------------------------------------------
str(assignment_data)

assignment_data$Experience <- as.factor(assignment_data$Experience)

levels(assignment_data$Experience)<-list("Yes"=1,"No"=2) #add meaningful level names to "Experience"


# Normality ---------------------------------------------------------------
psych::describe(assignment_data)

shapiro.test(assignment_data$Resilience) # is resilience normal?

# Homoegeneity of Variance ------------------------------------------------

library(onewaytests)
bf.test(Resilience ~ Experience, data = assignment_data) # brown-forscythe test 
# statistically significant, though we want it to be ns

psych::describeBy(assignment_data$Resilience, group = list(assignment_data$Experience))
# so that we can get SD for each Experience group on Resiliency

# Graphing ----------------------------------------------------------------
## Normality ----------------------------------------------------------------
normality_graph_resilience <- assignment_data %>%
  ggplot(aes(Resilience)) + 
  geom_histogram(aes(y = ..count..), binwidth = 5, fill = "white", color = "black") +
  theme_bw() +
  theme(axis.line= element_line(colour="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank())

ggsave(normality_graph_resilience, filename = "normality_graph_resilience.png")

qq_resilience <- qplot(sample = assignment_data$Resilience, stat = "qq") + 
  theme_bw() +
  theme(axis.line = element_line(colour="black"),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   panel.border=element_blank(),
                   panel.background=element_blank())

ggsave(qq_resilience, filename = "qq_resilience.png")

# Non-Parametric Tests ----------------------------------------------------
# Mann-Whitney/Wilcox test 
w_test <- wilcox.test(Resilience ~ Experience,
                      data = assignment_data,
                      paired = F,
                      conf.int = T,
                      conf.level = 0.95)
print(w_test)

# Effect size 
mann_whitney_z <- qnorm(w_test$p.value/2)
print(mann_whitney_z)

mann_whitney_r <- mann_whitney_z / sqrt(length(assignment_data$Resilience))
print(mann_whitney_r)

# Data Transformations ----------------------------------------------------
# Though, should only do if we have a theoretical reason to do so 
# Just know that you can use mutate command and add new variables instead of 
# duplicating the data frames

## Remove Outliers ---------------------------------------------------------
mean_blood_pressure <- mean(assignment_data$Resilience)

sd_blood_pressure <- sd(assignment_data$Resilience)

outlier_cutoff <- mean_blood_pressure + 2 * sd_blood_pressure
trimmed_data <- assignment_data
is.extreme <- trimmed_data$Resilience >= outlier_cutoff
trimmed_data$Resilience[is.extreme] <- NA
psych::describe(trimmed_data) # notice how skewness for Resilience is less

## Transform Scores ---------------------------------------------------------
transformed_data <- assignment_data
transformed_data$Resilience <- sqrt(transformed_data$Resilience) # because we had + skew, we use square root
psych::describe(transformed_data)




psych::describeBy(assignment_data$Resilience, group = list(assignment_data$Experience))


# Name: Rahul Patel
# Date: 2022-01-18
# Project: Assignment 1 
# Notes
 ## I will not be commenting on every command I wrote. That is a lot to ask. But, I have written some comments. 
 ## If you could give me feedback on what I should comment, that would be helpful. 

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(apaTables)
# Load Data Files ---------------------------------------------------------
pirate_data <- read_csv("correlationAssignmentData1.csv") # read_csv from tidyverse
social_data <- read_csv("correlationAssignmentData2.csv") # achieves same things done in class 

# Combine Data Files  -----------------------------------------------------
data_combined <- inner_join(pirate_data, social_data, by = "ID") # inner_join is similar to merge(); ID is the unique identifier
glimpse(data_combined) # to inspect the data
psych::describe(data_combined) # examine descriptive statistics

# Statistical Analyses ----------------------------------------------------

## Covariances -------------------------------------------------------------
cov_pirate_awkward <- cov(data_combined$Pirate, data_combined$Awkward) # use cov() from class
cov_pirate_extra <- cov(data_combined$Pirate, data_combined$Extra)

## Correlations ------------------------------------------------------------
# use corr.test from psych() package to test correlations 
corr_pirate_awkward <- psych::corr.test(data_combined$Pirate, data_combined$Awkward, method = "pearson", alpha = .05)
corr_pirate_awkward$ci.adj # Get the CI
corr_pirate_awkward$p.adj # Get the p-value

corr_pirate_extra <- psych::corr.test(data_combined$Pirate, data_combined$Extra, method = "pearson", alpha = .05)
corr_pirate_extra$ci.adj # Get the CI
corr_pirate_extra$p.adj # Get the p-value


## Regressions -------------------------------------------------------------
reg_pirate_extra <- lm(Extra ~ Pirate, data = data_combined) #lm() means linear model, and ~ means "predicted by"
summary(reg_pirate_extra)
apa.reg.table(reg_pirate_extra, filename = "reg_pirate_extra.doc") # export regression table as a word document


# Graphing ----------------------------------------------------------------
# First, get a prediction interval
pred_interval <- predict(reg_pirate_extra,
                         newdata = data_combined,
                         interval = "prediction",
                         level = .95) 

pirate_extra_data_pred <- cbind(data_combined, pred_interval) # combined the above prediction interval with existing data frame

# Where I plot the pirate and social extraness relation 
reg_plot_pirate_extra <- ggplot(data = pirate_extra_data_pred,
                   mapping = aes(x = Pirate, y = Extra)) +
  geom_point() + # add scatterplot
  geom_smooth(method = "lm", color = "red", se = TRUE) + # add regression line and confidence interval
  coord_cartesian(xlim = c(1, 6), ylim = c(1, 8)) +
  scale_y_continuous(breaks = seq(1, 8, by = 1)) + 
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  labs(x = "Pirate Self-Identification", y = "Social Extraness") + # add labels
  theme_classic(18) +
  geom_ribbon(mapping = aes(ymin = lwr, ymax = upr), # add prediction interval
              fill = "blue",
              alpha = .05) 
print(reg_plot_pirate_extra)

# Save graph as a pdf
ggsave(plot = reg_plot_pirate_extra, 
       filename = "reg_plot_pirate_extra.pdf", 
       width = 9, 
       height = 6, 
       dpi = "print")

# Save graph as a png
ggsave(plot = reg_plot_pirate_extra, 
       filename = "reg_plot_pirate_extra.png", 
       width = 9, 
       height = 6, 
       dpi = "print")

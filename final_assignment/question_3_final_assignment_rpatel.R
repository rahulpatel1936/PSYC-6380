# Name: Rahul Patel
# Date: April 10, 2022
# Project: Final Assignment

# Question 3  -------------------------------------------------------------

# Recode FocusType and DangerExp -----------------------------------------------

final_assignment_data_with_scores$FocusType <- as.factor(final_assignment_data_with_scores$FocusType)
final_assignment_data_with_scores$DangerExp <- as.factor(final_assignment_data_with_scores$DangerExp)

levels(final_assignment_data_with_scores$FocusType) <- list("PromotionFocus" = 1, "PreventionFocus" = 2)
levels(final_assignment_data_with_scores$DangerExp) <- list("No" = 1, "Yes" = 2)

# Set Options for ANOVA ---------------------------------------------------
options(contrasts = c("contr.helmert", "contr.poly"))

# Assumption Testing ------------------------------------------------------
final_assignment_data_with_scores$cells <- interaction(final_assignment_data_with_scores$FocusType, final_assignment_data_with_scores$DangerExp)

leveneTest(final_assignment_data_with_scores$risk_taking_propensity, group = final_assignment_data_with_scores$cells, center = "median")



# Descriptive Stats -------------------------------------------------------
psych::describeBy(final_assignment_data_with_scores$risk_taking_propensity, group = list(final_assignment_data_with_scores$FocusType))
psych::describeBy(final_assignment_data_with_scores$risk_taking_propensity, group = list(final_assignment_data_with_scores$DangerExp))
psych::describeBy(final_assignment_data_with_scores$risk_taking_propensity, group = list(final_assignment_data_with_scores$FocusType, final_assignment_data_with_scores$DangerExp))


# ANOVA -------------------------------------------------------------------

crf_lm <- lm(risk_taking_propensity ~ FocusType*DangerExp, data = final_assignment_data_with_scores)
Anova(crf_lm, type = 3)

eta_squared(crf_lm, partial = TRUE, ci = .95, alternative = "two.sided")

## Graph -------------------------------------------------------------------
library(skimr)
final_assignment_data_with_scores %>%
  skim()


risk_taking_propensity_line <- ggplot(final_assignment_data_with_scores,
                    mapping = aes(x = FocusType,
                                  y = risk_taking_propensity,
                                  shape = DangerExp,
                                  group = DangerExp,
                                  color = DangerExp)) +
  geom_jitter(position = position_jitterdodge(jitter.width = .5,
                                              dodge.width = .5),
              alpha = .5,
              size = 3) +
  stat_summary(fun.data =mean_cl_normal,
               geom = "point",
               position = position_dodge(width = 0.5),
               size = 3) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = 0.5),
               width = 0.5) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "line",
               position = position_dodge(width = 0.5),
               size = 1,
               mapping = aes(linetype = DangerExp)) +
  scale_color_manual(values=c("#dc3220","#005a5b")) + 
  coord_cartesian(ylim = c(1, 9)) +
  scale_y_continuous(breaks = seq(1, 9, by = 1)) +
  labs(x = "Focus Type", 
       y = "Risk-Taking Propensity", 
       linetype = "Experience of Danger",
       color = "Experience of Danger",
       shape = "Experience of Danger",
       caption = "Intervals indicate 95% CI") +
  theme_classic(18)

print(risk_taking_propensity_line)

ggsave(plot = risk_taking_propensity_line,
       height = 6,
       width = 8,
       filename = "risk_taking_propensity_line.jpg",
       dpi = "print")

## Simple Effects ----------------------------------------------------------
testInteractions(crf_lm, fixed = "DangerExp", across = "FocusType", adjustment = "none") # moderator: DangerExp, Predictor: FocusType

#APA Anova table, with interaction, and main effects
apa.aov.table(crf_lm,
              table.number = 5,
              filename = "table5.doc")

# Descriptives table, with cell and marginal means
# Makes the table but no CI's

apa.2way.table(iv1 = DangerExp,
               iv2 = FocusType,
               dv = risk_taking_propensity,
               show.marginal.means = TRUE,
               data = final_assignment_data_with_scores,
               table.number = 6,
               filename = "table6.doc")

#Calculate Cohen's d and its associated CIs using the "MBESS" package

# We can get the d-value for those with previous danger experinece with the code below
group1_yes<-dplyr::filter(final_assignment_data_with_scores,FocusType == "PromotionFocus",DangerExp == "Yes")$risk_taking_propensity
group2_yes<-dplyr::filter(final_assignment_data_with_scores,FocusType == "PreventionFocus",DangerExp == "Yes")$risk_taking_propensity

#Calculate Cohen's d and its associated CIs using the "MBESS" package
d.value_yes<-MBESS::smd(Group.1=group1_yes,Group.2=group2_yes)
MBESS::ci.smd(smd=d.value_yes,n.1=length(group1_yes),n.2=length(group2_yes))

# We can get the d-value for those with no previous danger experinece with the code below
group1_no<-dplyr::filter(final_assignment_data_with_scores,FocusType == "PromotionFocus",DangerExp == "No")$risk_taking_propensity
group2_no<-dplyr::filter(final_assignment_data_with_scores,FocusType == "PreventionFocus",DangerExp == "No")$risk_taking_propensity

#Calculate Cohen's d and its associated CIs using the "MBESS" package
d.value_no<-MBESS::smd(Group.1=group1_no,Group.2=group2_no)
MBESS::ci.smd(smd=d.value_no,n.1=length(group1_no),n.2=length(group2_no))





library(estimatr)
library(texreg)

displaced_hourly_1 <- lm_robust(change_wage ~ change_occ  + factor(year), data=displaced, weight=dwsuppwt)
displaced_hourly_2 <- lm_robust(change_wage ~ change_occ + downward_all_hourly + factor(year), data=displaced, weight=dwsuppwt)
displaced_hourly_3 <- lm_robust(change_wage ~ change_occ + downward_tasks_hourly + factor(year), data=displaced, weight=dwsuppwt)
displaced_hourly_4 <- lm_robust(change_wage ~ change_occ + downward_all_hourly + downward_tasks_hourly + factor(year), data=displaced, weight=dwsuppwt)

texreg(list(displaced_hourly_1, displaced_hourly_2, displaced_hourly_3, displaced_hourly_4), include.ci = FALSE, digits=3) %>%
    print(file = "./tex/regressions_DWS_group1.txt")

displaced_hourly_5 <- lm_robust(change_wage ~ change_occ + hellinger + factor(year), data=displaced, weight=dwsuppwt)
displaced_hourly_6 <- lm_robust(change_wage ~ change_occ + hellinger*downward_tasks_hourly + factor(year), data=displaced, weight=dwsuppwt)
displaced_hourly_7 <- lm_robust(change_wage ~ change_occ + hellinger*change_to_lss + factor(year), data=displaced, weight=dwsuppwt)

texreg(list(displaced_hourly_5, displaced_hourly_6, displaced_hourly_7), include.ci = FALSE, digits=3) %>%
    print(file = "./tex/regressions_DWS_group2.txt")
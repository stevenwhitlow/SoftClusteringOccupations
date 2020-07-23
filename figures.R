library(latex2exp)
library(extrafont)
library(ggalluvial)
library(viridis)
library(Hmisc)
library(texreg)
setwd("/Users/steven/Documents/School/Second Year/950/paper/")


ggplot(displaced, aes(x= (wage_task_hourly_current - wage_task_hourly_former), y=change_wage)) + 
  geom_point(mapping = aes(color = changed_mode)) + 
  guides(color = guide_legend(title=unname(TeX("$\\Delta w$")))) + 
  labs(x = "Wage change predicted by type measures", y = "Actual wage change") +
  theme_bw() + theme(text=element_text(size=16, family="Palatino"))
ggsave("./figures/change_wage.pdf")

classifications %>% 
  mutate(mode = pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7)) %>%
  mutate(rank = rank(mode)) %>%
  arrange(rank) -> classifications_figures_df

ggplot(classifications_figures_df, aes(x=mode)) +
  stat_ecdf(geom = "step", size = 1) +
  labs(x = "Degree of membership of primary type", y = "Cumulative probability") +
  theme_bw() +
  theme(text=element_text(size=16, family="Palatino"))
ggsave("./figures/degree_of_mixing.pdf")

displaced %>%
  filter(mode_current!="NA" & mode_former!="NA") %>%
  group_by(mode_former, mode_current) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) -> sankey_data

svy_displaced <- as_survey(displaced, probs = dwsuppwt, nest = TRUE)
mode_and_wage_changes <- svy_displaced %>% ungroup() %>%
  filter(mode_former!="NA" & mode_current!="NA") %>%
  select(change_wage, mode_former, mode_current, dwsuppwt) %>%
  group_by(mode_former, mode_current) %>%
  summarise(change_wage = survey_mean(change_wage), change_wage_sd = survey_var(change_wage), total = survey_total(1))

#Convert total into percentages and get SD instead of variance
mode_and_wage_changes <- mode_and_wage_changes %>%
  mutate(total = total/(select(mode_and_wage_changes, total) %>% sum()),
         change_wage_sd = sqrt(change_wage_sd))

switch_means <- mode_and_wage_changes %>%
  select(mode_current, mode_former,change_wage) %>%
  pivot_wider(names_from = mode_current, values_from = change_wage)
switch_sds <- mode_and_wage_changes %>%
  select(mode_current, mode_former,change_wage_sd) %>%
  pivot_wider(names_from = mode_current, values_from = change_wage_sd)

means_table <- list()
for (j in 2:ncol(switch_means)) {
  means_table[[j-1]] <- createTexreg(
    coef.names = switch_means[[1]],
    coef = switch_means[[j]],
    se = switch_sds[[j]]
  )
}
texreg(means_table, custom.model.names = switch_means[[1]], digits=3) %>% print(file = "./tex/switch_means.txt")

ggplot(inner_join(sankey_data,mode_and_wage_changes),
       aes(axis1 = mode_former,
           axis2 = mode_current,
           y = total)) +
  geom_alluvium(aes(fill = change_wage)) +
  geom_stratum(alpha = 0.1) +
  geom_text(stat = "stratum", 
            label.strata = TRUE) +
  scale_x_discrete(limits = c("Pre-displacement \n primary type", "Post-displacement \n primary type"),
                   expand = c(-0.3, 0.3)) +
  guides(fill = guide_legend(title=unname(TeX("$\\Delta w$")))) +
  labs(y = "Share of total") +
  theme_minimal() + scale_fill_fermenter(palette="YlGnBu", limits = c(-0.3, 0.3)) + #scale_fill_viridis_b(limits = c(-0.3, 0.3)) +
  theme(text=element_text(size=16, family="Palatino"), legend.position = "bottom")
ggsave("./figures/switching_post_displacement.pdf", width = 11, height = 8.5, units = "in")


#####
library(ggtern)

svy_census <- as_survey(census, probs = perwt, nest = TRUE)
svy_census <- svy_census %>% filter(perwt != 0)
classifications_simplex <- classifications %>% inner_join(svy_census %>% select(occnumber, perwt) %>% group_by(occnumber) %>% summarise(perwt = survey_total(1)))

#Generate mean wages by occupation
classifications_simplex <- classifications_simplex %>% inner_join(svy_census %>% select(occnumber, perwt, lnhrwage) %>% group_by(occnumber) %>% summarise(lnhrwage = survey_mean(lnhrwage)))

#Generate cross-sectional mean wages
census_mean_wage <- svy_census %>% select(occnumber, perwt, lnhrwage) %>% summarise(lnhrwage = survey_mean(lnhrwage))

#Generate relative wage
classifications_simplex <- classifications_simplex %>%
  mutate(relwage = lnhrwage - (census_mean_wage$lnhrwage))

#Calculate the share of the total workforce working in a given occupation (note: share of workers, not hours)
classifications_simplex <- classifications_simplex %>%
  mutate(percent_workforce = perwt/(select(classifications_simplex, perwt) %>% sum()))

#Get the share of workers in 2000 who have a significant degree of mixing over pure types.
classifications_simplex %>% filter(freq1<0.95 & freq2<0.95 & freq3<0.95 & freq4<0.95 & freq5 <0.95 & freq6<0.95 & freq7<0.95) %>% summarise(sum(percent_workforce))

classifications_simplex %>% 
  mutate(freq1_r = round(freq1 + freq2, 2)) %>%
  mutate(freq2_r = round(freq3 + freq4 + freq5, 2)) %>%
  mutate(freq3_r = round(freq6 + freq7, 2)) -> classifications_simplex

#Get the share of workers in 2000 who have a significant degree of mixing over pure types, after aggregating the 7 pure types into 3 broad groups.
classifications_simplex %>% filter(freq1_r<0.95 & freq2_r<.95 & freq3_r<.95) %>% summarise(sum(percent_workforce))

classifications_simplex %>% select(freq1_r, freq2_r, freq3_r, percent_workforce, relwage) %>% fgroup_by(freq1_r,freq2_r,freq3_r) %>% collapg(w = percent_workforce) %>%
  ggtern(aes(freq1_r,freq2_r,freq3_r,value=relwage)) +
  #stat_interpolate_tern(geom="polygon",
  #                      formula=value~x+y,
  #                      method=lm,n=100,
  #                      breaks=seq(1.75,3.75,by=0.04),
  #                      aes(fill=..level..), expand=1) + scale_fill_viridis() +
  geom_mask() +
  theme_bw() +
  theme_showarrows() +
  theme_clockwise() +
  labs(x="Abstract", y="Mixed", z="Low-Skill") +
  geom_point(aes(size=percent_workforce*100, color=relwage)) + scale_size_area(max_size = 15) +
  guides(color=guide_legend(title=unname(TeX("$E(w - \\bar{w})$"))), size=guide_legend(title="Employment (%)")) + 
  scale_colour_distiller(palette="YlGnBu") + #scale_color_viridis() +
  #geom_text(aes(label=ifelse(Title=="First-Line Supervisors of Retail Sales Workers",
  #                          as.character(Title),'')),hjust=0,vjust=0, size=3) +
  theme(text=element_text(size=16, family="Palatino"), legend.position = "right")
ggsave("./figures/simplex_rounded.pdf", width = 11, height = 8.5, units = "in")

# ggtern(data=classifications_simplex,aes(freq1_r,freq23_r,freq4_r,value=lnhrwage)) + 
#   geom_mask() +
#   theme_bw() +
#   theme_showarrows() +
#   theme_clockwise() +
#   labs(x="Interpersonal", y="Manual", z="Cognitive") +
#   stat_interpolate_tern(geom="polygon",
#                         formula=value~x+y,
#                         method=lm,n=100,
#                         breaks=seq(1.75,3.75,by=0.04),
#                         aes(fill=..level..), expand=1)  + scale_fill_viridis() +
#   geom_count(aes(size=percent_workforce*100)) + scale_size_area(max_size = 6.5) +
#   guides(fill=guide_legend(title=unname(TeX("$E(w)$"))), size=guide_legend(title="Employment (%)")) +
#   theme(text=element_text(size=16, family="Palatino"))
# ggsave("simplex_alternate.pdf")
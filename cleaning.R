
library(tidyverse)
library(lmtest)
library(sandwich)
library(readstata13)
library(survey)
library(srvyr)
setwd("/Users/steven/Documents/research/occupations/")

##
## DWS 02,04,06,08,10
##

crosswalk <- read.dta13("/Users/steven/Downloads/Data_and_code/Data/cens00_soc_xwalk_sanders.dta")
classifications <- read.csv("classifications.csv")
crosswalk <- full_join(classifications,crosswalk, by = (c("O.NET.SOC.Code"="onetsoccode")))
crosswalk <- subset(crosswalk, select = -c(title))
displaced <- read.dta13("displaced.dta") 
ranks <- read.dta13("ranks.dta") 
displaced %>% 
  filter(!(occ==0) & dwwagec!=0 & dwwagel!=0 & dwweekc!=0 & dwweekl!=0) %>% 
  filter(!(dwweekc >= 9999.96 & dwweekl <= 9999.99)) %>%
  filter(!(dwweekc >= 9999.96 & dwweekl <= 9999.99)) %>%
  filter(!(dwwagec >= 99.96 & dwwagel <= 99.99)) %>%
  filter(!(dwwagel >= 99.96 & dwwagel <= 99.99)) %>%
  mutate(occ = (occ/10)) %>%
  mutate(dwocc = (dwocc/10)) -> displaced

displaced <- full_join(displaced,crosswalk, by = (c("occ"="occ00")))
displaced <- full_join(displaced,crosswalk, by = (c("dwocc"="occ00")), suffix = c("_current", "_former"))

displaced <- full_join(displaced,ranks, by = (c("occ"="occ00")))
displaced <- full_join(displaced,ranks, by = (c("dwocc"="occ00")), suffix = c("_current", "_former"))

#Only keep displaced workers
displaced %>% filter(dwstat=="displaced worker") -> displaced

#Generate dummy variables for reg
displaced %>%
  mutate(change_occ = ifelse((occ!=dwocc), 1, 0)) %>%
  mutate(downward_all_hourly = ifelse((rank_all_hourly_current < rank_all_hourly_former), 1, 0)) %>%
  mutate(downward_tasks_hourly = ifelse((rank_task_hourly_current < rank_task_hourly_former), 1, 0)) %>%
  mutate(downward_resid_hourly = ifelse((rank_resid_hourly_current < rank_resid_hourly_former), 1, 0)) %>%
  mutate(downward_all_weekly = ifelse((rank_all_weekly_current < rank_all_weekly_former), 1, 0)) %>%
  mutate(downward_tasks_weekly = ifelse((rank_task_weekly_current < rank_task_weekly_former), 1, 0)) %>%
  mutate(downward_resid_weekly = ifelse((rank_resid_weekly_current < rank_resid_weekly_former), 1, 0)) -> displaced

#Education variables
displaced <- displaced %>%
  mutate(dropout = ifelse((as.numeric(educ) <= 19), 1, 0),
         hs_grad = ifelse((as.numeric(educ) > 19 & as.numeric(educ) < 23), 1, 0),
         some_college = ifelse((as.numeric(educ) > 21 & as.numeric(educ) < 29), 1, 0),
         college = ifelse((as.numeric(educ) >= 29), 1, 0))

#Education variables
displaced <- displaced %>%
  mutate(plant_closed = ifelse((as.numeric(dwreas) == 1), 1, 0),
         insf_work = ifelse((as.numeric(dwreas) == 2), 1, 0))

#Generat euclidian distance
displaced %>% mutate(euclid = 0) -> displaced
displaced %>% mutate(hellinger = 0) -> displaced
for (num_group in 1:7){
  var_new <- paste0("freq", num_group, "_current")
  var_old <- paste0("freq", num_group, "_former")
  displaced %>% 
    mutate(euclid = euclid + sqrt(((eval(parse(text = var_new))) - (eval(parse(text = var_old))))^2)) -> displaced
  displaced %>%
    mutate(hellinger = hellinger + (sqrt( eval(parse(text = var_new)) ) - sqrt( eval(parse(text = var_old)) ) )^2 )  -> displaced
}
displaced %>%
  mutate(hellinger = (1/sqrt(2))*sqrt(hellinger)) -> displaced

displaced %>% mutate(mode_current = case_when(
                       freq1_current == pmax(freq1_current, freq2_current, freq3_current, freq4_current, freq5_current, freq6_current, freq7_current) ~ "1: Scientific",
                       freq2_current == pmax(freq1_current, freq2_current, freq3_current, freq4_current, freq5_current, freq6_current, freq7_current) ~ "2: Other Abstract",
                       freq3_current == pmax(freq1_current, freq2_current, freq3_current, freq4_current, freq5_current, freq6_current, freq7_current) ~ "3: Skilled Manual",
                       freq4_current == pmax(freq1_current, freq2_current, freq3_current, freq4_current, freq5_current, freq6_current, freq7_current) ~ "4: Skilled Service",
                       freq5_current == pmax(freq1_current, freq2_current, freq3_current, freq4_current, freq5_current, freq6_current, freq7_current) ~ "5: Routine Cognitive",
                       freq6_current == pmax(freq1_current, freq2_current, freq3_current, freq4_current, freq5_current, freq6_current, freq7_current) ~ "6: Low-Skill Manual",
                       freq7_current == pmax(freq1_current, freq2_current, freq3_current, freq4_current, freq5_current, freq6_current, freq7_current) ~ "7: Low-Skill Service",
                       )) -> displaced

displaced %>% mutate(mode_former = case_when(
                       freq1_former == pmax(freq1_former, freq2_former, freq3_former, freq4_former, freq5_former, freq6_former, freq7_former) ~ "1: Scientific",
                       freq2_former == pmax(freq1_former, freq2_former, freq3_former, freq4_former, freq5_former, freq6_former, freq7_former) ~ "2: Other Abstract",
                       freq3_former == pmax(freq1_former, freq2_former, freq3_former, freq4_former, freq5_former, freq6_former, freq7_former) ~ "3: Skilled Manual",
                       freq4_former == pmax(freq1_former, freq2_former, freq3_former, freq4_former, freq5_former, freq6_former, freq7_former) ~ "4: Skilled Service",
                       freq5_former == pmax(freq1_former, freq2_former, freq3_former, freq4_former, freq5_former, freq6_former, freq7_former) ~ "5: Routine Cognitive",
                       freq6_former == pmax(freq1_former, freq2_former, freq3_former, freq4_former, freq5_former, freq6_former, freq7_former) ~ "6: Low-Skill Manual",
                       freq7_former == pmax(freq1_former, freq2_former, freq3_former, freq4_former, freq5_former, freq6_former, freq7_former) ~ "7: Low-Skill Service",
                       )) -> displaced

displaced %>% mutate(changed_mode = case_when(
  mode_current == mode_former ~ 0,
  mode_current != mode_former ~ 1,
  ))-> displaced

#Generate change in wages 

#Read in CPI data
cpi <- read.csv("CPIAUCSL.csv")
require(lubridate)
cpi <- cpi %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(year = year(DATE)) %>%
  mutate(month = month(DATE))
cpi <- cpi %>%
  group_by(year) %>%
  mutate(cpi_year = mean(CPIAUCSL))
cpi_years <- cpi %>%
  filter(month == 1) %>%
  select(year, cpi_year) %>%
  rename(cpi_displaced = cpi_year)
cpi_jan <- cpi %>%
  filter(month==1) %>%
  select(year, CPIAUCSL) %>%
  rename(cpi_jan = CPIAUCSL)

#Deflate wages:

displaced <- displaced %>%
  mutate(year_displaced = case_when(
    dwlastwrk == "last year" ~ year - 1,
    dwlastwrk == "two years ago" ~ year - 2,
    dwlastwrk == "three years ago" ~ year - 3,
  ))

displaced <- displaced %>%
  inner_join(cpi_jan, by=(c("year"="year"))) %>%
  inner_join(cpi_years, by=(c("year_displaced"="year")))

displaced <- displaced %>% mutate(new_wage = log(dwwagec/cpi_jan*100))
displaced <- displaced %>% mutate(old_wage = log(dwwagel/cpi_displaced*100))
displaced %>% mutate(change_week = log(dwweekc/cpi_jan*100) - log(dwweekl/cpi_displaced*100)) -> displaced
displaced %>% mutate(change_wage = log(dwwagec/cpi_jan*100) - log(dwwagel/cpi_displaced*100)) -> displaced
#########

displaced %>%
  filter(!(dwwagec<5.15*0.75 & year<2008)) %>%
  filter(!(dwwagec<5.85*0.75 & year==2008)) %>%
  filter(!(dwwagec<7.25*0.75 & year==2010)) -> displaced

displaced %>%
  filter(!(dwwagel<5.15*0.75 & year_displaced<=2007)) %>%
  filter(!(dwwagel<5.85*0.75 & year_displaced<=2008)) %>%
  filter(!(dwwagel<6.55*0.75 & year_displaced<=2009)) -> displaced

displaced %>%
  mutate(change_occ = ifelse((occ!=dwocc), 1, 0)) %>%
  mutate(change_occ_2digit = ifelse((floor(occ/10)!=floor(dwocc/10)), 1, 0)) %>%
  mutate(predicted_task_hourly = wage_task_hourly_current - wage_task_hourly_former) %>%
  mutate(predicted_resid_hourly = wage_resid_hourly_current - wage_resid_hourly_former) -> displaced


displaced <- displaced %>% mutate(quartile = ntile(change_wage, 4))
displaced <- displaced %>% group_by(year) %>% mutate(quartile_year = ntile(change_wage, 4)) %>% ungroup()
displaced %>%
  group_by(year) %>%
  summarise(mean_task = mean(downward_tasks_hourly, na.rm=TRUE), mean_resid = mean(downward_resid_hourly, na.rm=TRUE))

displaced %>%
  mutate(change_to_lss_primary = ifelse((mode_current=="7: Low-Skill Service" & mode_former!="7: Low-Skill Service"), 1, 0)) %>%
  mutate(change_to_lss = ifelse((freq7_current > freq7_former), 1, 0)) %>%
  mutate(lss_primary = ifelse((mode_current=="7: Low-Skill Service"), 1, 0)) -> displaced

displaced <- displaced %>% mutate(downward_lss = downward_tasks_hourly*(change_to_lss),
                                  downward_not_lss = downward_tasks_hourly*(1-change_to_lss),
                                  downward_lss_alt = downward_all_hourly*(change_to_lss),
                                  downward_not_lss_alt = downward_all_hourly*(1-change_to_lss),
                                  downward_lss_primary = downward_tasks_hourly*(lss_primary),
                                  downward_not_lss_primary = downward_tasks_hourly*(1-lss_primary))

displaced <- displaced %>% filter(mode_current!="NA" & mode_former!="NA")

##
## Census 2000
##

census <- read.dta13("usa_00032.dta") 
census %>% filter((wkswork1>35 & as.integer(uhrswork)>35)) -> census
census %>% mutate(lnhrwage = log(incwage/(wkswork1*as.integer(uhrswork)))) -> census
census %>% filter(!(incwage==0)) -> census
crosswalk <- read.dta13("/Users/steven/Downloads/Data_and_code/Data/cens00_soc_xwalk_sanders.dta")
crosswalk <- right_join(classifications,crosswalk, by = (c("O.NET.SOC.Code"="onetsoccode")))
crosswalk <- subset(crosswalk, select = -c(title))
census <- left_join(census,crosswalk, by = (c("occ"="occ00")))

census %>% mutate(modal_group = case_when(
  freq1 == pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7) ~ 1,
  freq2 == pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7) ~ 2,
  freq3 == pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7) ~ 3,
  freq4 == pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7) ~ 4,
  freq5 == pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7) ~ 5,
  freq6 == pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7) ~ 6,
  freq7 == pmax(freq1, freq2, freq3, freq4, freq5, freq6, freq7) ~ 7,
)) -> census

census_baseline <- lm(lnhrwage ~ as.factor(occ), na.action = na.omit, data=census)
coeftest(census_baseline, vcov = vcovHC(census_baseline, type = 'HC1'))
summary(census_baseline)